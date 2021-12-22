package maf.aam.scheme.stores

import maf.aam.scheme.BaseSchemeAAMSemantics
import maf.aam.{AAMGraph, BaseSimpleWorklistSystem, GraphElementAAM}
import maf.core.{Address, BasicStore, Lattice}

object DepLoggingStore:
    def from[V: Lattice](sto: BasicStore[Address, V]): DepLoggingStore[V] =
      DepLoggingStore(sto, Map().withDefaultValue(List()), Set())

/** A logging store that keeps track of the read dependencies in addition to the changes that it makes. */
case class DepLoggingStore[V: Lattice](originalSto: BasicStore[Address, V], W: Map[Address, List[V]], R: Set[Address]):
    /** Replays the store against the given global store. Returns the set of addresses that it has affected */
    def replay(store: BasicStore[Address, V]): (BasicStore[Address, V], Set[Address]) =
        val (sto1, writes) = W.foldLeft((store, Set[Address]())) { case ((store, writes), (adr, vlus)) =>
          val before = store.lookup(adr)
          val newStore = vlus.foldLeft(store)((sto, vlu) => sto.extend(adr, vlu))
          (newStore, if before != newStore.lookup(adr) then writes + adr else writes)
        }

        (sto1, writes)

    /** Append the given write to the log */
    def append(a: Address, value: V): DepLoggingStore[V] =
      this.copy(W = W + (a -> (value :: W(a))))

    /** Returns true if the address is already part of the log */
    def contains(a: Address): Boolean = W.contains(a)

    /** Returns the value from the log */
    def lookup(addr: Address): V =
      W.get(addr)
        .getOrElse(List())
        .foldLeft(Lattice[V].bottom)((joined, vlu) => Lattice[V].join(joined, vlu))

    /** Register a read dependency from the current state to the given address */
    def register(readDep: Address): DepLoggingStore[V] =
      this.copy(R = R + readDep)

    def readDeps: Set[Address] = R

/** A logging store were we also keep track of the dependencies, so that components are not re-analyzed unnecessarily */
trait BaseSchemeDependencyLoggingStore extends BaseSchemeAAMSemantics, BaseSimpleWorklistSystem:
    type Sto = DepLoggingStore[Storable]
    type Conf = SchemeConf
    type System = DepLoggingStoreSystem

    private var originalSto = BasicStore(initialBds.map(p => (p._2, p._3)).toMap).extend(Kont0Addr, Storable.K(Set(HltFrame)))
    private var sto: BasicStore[Address, Storable] = originalSto
    private var logs: List[DepLoggingStore[Storable]] = List()

    class DepLoggingStoreSystem extends SeenStateSystem:
        /** A global version of the R set (read dependencies) */
        private var R: Map[Address, Set[Conf]] = Map().withDefaultValue(Set())

        /** Replay the logs from all the collected stores in the global store, and return the set of address that were written */
        private def appendAll(logs: List[DepLoggingStore[Storable]]): Set[Address] =
            val (sto1, writes) = logs.foldLeft((sto, Set[Address]())) { case ((sto, writes), log) =>
              val (sto1, writes1) = log.replay(sto)
              (sto1, writes ++ writes1)
            }

            sto = sto1
            writes

        override def popWork(): Option[(Option[Conf], Conf)] =
            println(s"B: work size ${work.size} and newWork size ${newWork.size}")
            if work.isEmpty && newWork.nonEmpty then
                // we exhausted our previous worklist lets get started on the frontier (the successors of the previous worklist)
                val writes = appendAll(logs)
                // we may now forget the logs
                logs = List()
                // now only add those states in the frontier to the worklist that are actually affected by the change
                val tmpWorkList = newWork.filter { case (_, conf) =>
                  writes.exists(a => R(a).contains(conf)) || !seen.contains(conf)
                }

                tmpWorkList.map(_._2).foreach(addSeen)
                (newWork.toSet -- tmpWorkList.toSet).foreach { (from, to) => addBump(from.get, to) }
                println(s"Before filtering ${newWork.size} and after filtering ${tmpWorkList.size}")

                work = tmpWorkList
                newWork = List()

            println(s"A: work size ${work.size} and newWork size ${newWork.size}")
            super.popWork()

        def register(conf: Conf, a: Address): Unit =
          R = R + (a -> (R(a) + conf))

    case class SchemeConf(c: Control, k: Address | Frame, t: Timestamp, extra: Ext)

    override def decideSuccessors[G](depGraph: G, prev: Conf, successors: Set[State], sys: System)(using AAMGraph[G]): (System, G) =
        successors.foreach { successor =>
            val conf = asConf(successor, sys)
            sys.pushWork(Some(prev), conf)
            logs = successor.s :: logs
            println(s"Read deps ${successor.s.readDeps}")
            successor.s.readDeps.foreach(sys.register(prev, _))
        }

        (sys, depGraph)

    override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
      sto.append(addr, value)

    override def readSto(sto: Sto, addr: Address): (Storable, Sto) =
      if sto.contains(addr) then
          // look in the log for the value, this is different in the Optimizing AAM paper
          // as we assume there that the invriant holds that states do not read from the log
          (sto.lookup(addr), sto)
      else (sto.originalSto.lookup(addr).getOrElse(Storable.V(lattice.bottom)), sto.register(addr))

    override def asState(conf: Conf, sys: System): State =
      SchemeState(conf.c, DepLoggingStore.from(sto), conf.k, conf.t, conf.extra)

    override def asConf(state: State, sys: System): Conf =
      SchemeConf(state.c, state.k, state.t, state.extra)

    override def injectConf(e: Expr): Conf =
      SchemeConf(Control.Ev(e, initialEnv), Kont0Addr, initialTime, emptyExt)

    override def inject(e: Expr): System =
      DepLoggingStoreSystem().pushWork(None, injectConf(e))

    override def asGraphElement(c: Conf, sys: System): GraphElementAAM =
      asGraphElement(c.c, c.k, DepLoggingStore.from(sto), c.extra, c.hashCode)

    override lazy val initialStore: Sto = DepLoggingStore.from(originalSto)
