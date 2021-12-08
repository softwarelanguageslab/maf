package maf.aam.scheme

import maf.core.{Address, BasicStore, Lattice}
import maf.aam.BaseSimpleWorklistSystem

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

trait BaseSchemeLoggingLocalStore extends BaseSchemeAAMSemantics, BaseSimpleWorklistSystem:
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
        var sto: BasicStore[Address, Storable] = BasicStore(Map())
        var logs: Set[LoggingStore[Storable]] = Set()

        private def appendAll(logs: Set[LoggingStore[Storable]]): Boolean =
            val (sto1, changed) = logs.foldLeft((sto, false)) { case ((sto, hasChanged), log) =>
              val (sto1, hasChanged1) = log.replay(sto)
              (sto1, hasChanged || hasChanged1)
            }
            sto = sto1
            changed

        /** When we serve work, we check whether the changes to the store need to be applied */
        override def popWork(): Option[Conf] =
            if work.isEmpty && newWork.nonEmpty then
                change {
                  // if the current "turn" is finished we can apply the logs together
                  // into the global store.
                  val hasChanged = appendAll(logs)
                  // we may now forget the logs
                  logs = Set()
                  // increase the `tt` value if the predecessors caused changes to the store
                  newWork = (if hasChanged then newWork.map(conf => conf.copy(tt = conf.tt + 1))
                             else newWork).filter(!seen.contains(_))
                  // mark the ones scheduled to be analyzed as seen.
                  newWork.foreach(conf => seen = seen + conf)
                }
            super.popWork()

        def addLog(log: LoggingStore[Storable]): Unit =
          logs = logs + log

    override def asState(conf: Conf, sys: System): State =
      SchemeState(conf.c, LoggingStore(sys.sto), conf.k, conf.t, conf.extra)

    override protected def integrate(st: State, sys: System): (Conf, System) =
        sys.addLog(st.s)
        super.integrate(st, sys)

    override protected def decideSuccessors(successors: Set[State], sys: System): System =
        successors.foreach { successor =>
            val (conf, sys1) = integrate(successor, sys)
            sys1.pushWork(conf)
        }

        sys

    override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
      sto.append(addr, value)

    override def readSto(sto: Sto, addr: Address): Storable =
        if sto.contains(addr) then println(s"WARN: potentially trying to read from the log at address $addr, returning old value")

        sto.originalSto.lookup(addr).getOrElse(Storable.V(lattice.bottom))
