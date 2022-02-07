package maf.aam.scheme.stores

import maf.aam.scheme.*
import maf.language.scheme.SchemeExp
import maf.core.{Address, BasicStore, Lattice}
import maf.aam.{AAMGraph, BaseSimpleWorklistSystem, GraphElementAAM}

case class LoggingStore[V: Lattice](originalSto: BasicStore[Address, V], changes: Map[Address, List[V]] = Map()):
    def replay(store: BasicStore[Address, V]): (BasicStore[Address, V], Boolean) =
        val (sto1, hasChanged) = changes.foldLeft((store, false)) { case ((store, hasChanged), (adr, vlus)) =>
          val before = store.lookup(adr)
          val newStore = vlus.foldLeft(store)((sto, vlu) => sto.extend(adr, vlu))
          (newStore, hasChanged || before != newStore.lookup(adr))
        }
        (sto1, hasChanged)

    def append(addr: Address, v: V): LoggingStore[V] =
      this.copy(changes = changes + (addr -> (v :: changes.get(addr).getOrElse(List()))))

    def contains(addr: Address): Boolean = changes.contains(addr)

    def lookup(addr: Address): V =
      changes
        .get(addr)
        .getOrElse(List())
        .foldLeft(Lattice[V].bottom)((joined, vlu) => Lattice[V].join(joined, vlu))

trait BaseSchemeLoggingLocalStore extends BaseSchemeAAMSemantics, BaseSimpleWorklistSystem[SchemeExp]:
    type Sto = LoggingStore[Storable]
    type System = LoggingLocalStoreSystem
    type Conf = SchemeConf

    /**
     * The worklist algorithm only needs to consider states without stores. This greatly reduces the amount of states required.
     *
     * @param c
     *   the control component (either Ev or Ap)
     * @param k
     *   a reference the continuation, which is either allocated in the store, or just linked directly
     * @param t
     *   the timestamp (for use with k-cfa)
     * @param extra
     *   any additional data that should be kept in the configuration
     * @param tt
     *   the size of the store chain. It is incremented each time the global store changes to determine whether a configuration needs to be reanalyzed
     *   without keeping track of the entire store in the configuration.
     */
    case class SchemeConf(c: Control, k: Address | Frame, t: Timestamp, extra: Ext, tt: Int)

    class LoggingLocalStoreSystem extends SeenStateSystem:
        /** Imperative global store */
        var sto: BasicStore[Address, Storable] = originalSto
        var logs: Set[LoggingStore[Storable]] = Set()
        var tt: Int = 0

        private def appendAll(logs: Set[LoggingStore[Storable]]): Boolean =
            val (sto1, changed) = logs.foldLeft((sto, false)) { case ((sto, hasChanged), log) =>
              val (sto1, hasChanged1) = log.replay(sto)
              (sto1, hasChanged || hasChanged1)
            }
            sto = sto1
            changed

        /** When we serve work, we check whether the changes to the store need to be applied */
        override def popWork(): Option[(Option[Conf], Conf)] =
            if work.isEmpty && newWork.nonEmpty then
                // if the current "turn" is finished we can apply the logs together
                // into the global store.
                val hasChanged = appendAll(logs)
                // we may now forget the logs
                logs = Set()
                // increase the `tt` value if the predecessors caused changes to the store
                val tmpWork =
                  (if hasChanged then newWork.map(conf => (conf._1, conf._2.copy(tt = conf._2.tt + 1)))
                   else newWork)

                newWork = List()
                // mark the ones scheduled to be analyzed as seen.
                tmpWork.foreach { conf =>
                  if !seen.contains(conf._2) then
                      seen = seen + conf._2
                      newWork = conf :: newWork
                  else increment(Bump) // logging
                }

                report(Seen, seen.size) // logging

                tt = tt + (if hasChanged then 1 else 0)
            super.popWork()

        def addLog(log: LoggingStore[Storable]): Unit =
          logs = logs + log

    private var originalSto = BasicStore(initialBds.map(p => (p._2, p._3)).toMap).extend(Kont0Addr, Storable.K(Set(HltFrame)))

    override def asState(conf: Conf, sys: System): State =
      SchemeState(conf.c, LoggingStore(sys.sto), conf.k, conf.t, conf.extra)

    override def asConf(state: State, sys: System): Conf =
      SchemeConf(state.c, state.k, state.t, state.extra, sys.tt)

    override def injectConf(e: Expr): Conf =
      SchemeConf(Control.Ev(e, initialEnv), Kont0Addr, initialTime, emptyExt, 0)

    override def inject(e: Expr): System =
      LoggingLocalStoreSystem().pushWork(None, injectConf(e))

    override lazy val initialStore: Sto = LoggingStore(originalSto)

    override protected def decideSuccessors[G](depGraph: G, prev: Conf, successors: Set[State], sys: System)(using AAMGraph[G]): (System, G) =
        successors.foreach { successor =>
            val conf = asConf(successor, sys)
            sys.pushWork(Some(prev), conf)
            sys.addLog(successor.s)
        }

        (sys, depGraph)

    override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
      sto.append(addr, value)

    override def readSto(sto: Sto, addr: Address): (Storable, Sto) =
      if sto.contains(addr) then
          // look in the log for the value, this is different in the Optimizing AAM paper
          // as we assume there that the invriant holds that states do not read from the log
          (sto.lookup(addr), sto)
      else (sto.originalSto.lookup(addr).getOrElse(Storable.V(lattice.bottom)), sto)

    override def asGraphElement(c: Conf, sys: System): GraphElementAAM =
      asGraphElement(c.c, c.k, LoggingStore(sys.sto), c.extra, c.hashCode)
