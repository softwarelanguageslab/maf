package maf.aam.scheme.stores

import maf.core.*
import scala.annotation.static
import maf.aam.{AAMAnalysis, AAMGraph, GraphElementAAM}
import maf.aam.scheme.BaseSchemeAAMSemantics
import maf.util.Trampoline.run
import scala.collection.mutable.Queue

/** An effect driven imperative store */
trait SchemeImperativeStoreWidening extends AAMAnalysis, BaseSchemeAAMSemantics:
    case class SchemeConf(c: Control, k: KonA, t: Timestamp, extra: Ext)
    type Conf = SchemeConf
    type Sto = EffectSto
    override type System = EffectDrivenAnalysisSystem

    private var originalSto = BasicStore(initialBds.map(p => (p._2, p._3)).toMap).extend(Kont0Addr, Storable.K(Set(HltFrame)))
    var store: BasicStore[Address, Storable] = originalSto

    override def asGraphElement(c: Conf, sys: System): GraphElementAAM =
      asGraphElement(c.c, c.k, EffectSto(Set(), Set()), c.extra, c.hashCode)

    override lazy val initialStore: Sto = EffectSto(Set(), Set())

    case class EffectSto(w: Set[Address], r: Set[Address]):
        def writeDep(a: Address): EffectSto =
          this.copy(w = w + a)

        def readDep(a: Address): EffectSto =
          this.copy(r = r + a)

    class EffectDrivenAnalysisSystem extends BaseSystem:
        def allConfs: Set[Conf] = seen

        def finalStates: Set[State] =
          seen.map(asState(_, this)).filter(isFinal)

        private var seen: Set[Conf] = Set()
        private var R: Map[Address, Set[Conf]] = Map().withDefaultValue(Set())
        private val worklist: Queue[Conf] = Queue()

        def popWork(): Option[Conf] =
          if worklist.nonEmpty then
              val conf = worklist.dequeue
              Some(conf)
          else None

        /** Trigger some read dependencies */
        def trigger(w: Set[Address]): Unit =
          change {
            w.foreach(adr => R(adr).foreach(worklist.enqueue))
          }

        /** Register a read dependency */
        def register(r: Set[Address], conf: Conf): Unit =
          change {
            r.foreach(adr => R = R + (adr -> (R(adr) + conf)))
          }

        /** Spawn a configuration */
        def spawn(conf: Conf): Unit =
          if !seen.contains(conf) then
              change {
                seen = seen + conf
                worklist.enqueue(conf)
              }

    override def writeSto(sto: Sto, a: Address, value: Storable): Sto =
        // directly write it to the global store
        val oldValue = store.lookup(a)
        store = store.extend(a, value)
        if oldValue == store.lookup(a) then sto else sto.writeDep(a)

    override protected def transition[G](system: System, dependencyGraph: G)(using g: AAMGraph[G]): (System, G) =
        val workOption = system.popWork()
        if workOption.isEmpty then (system, dependencyGraph)
        else
            val work = workOption.get
            val successors = run(step(asState(work, system)))
            successors.foreach { next =>
                val sto = next.s
                val nextConf = asConf(next, system)
                system.spawn(nextConf)
                system.register(sto.r, work)
                system.trigger(sto.w)
            }

            (system, dependencyGraph)

    override def readSto(sto: Sto, addr: Address): (Storable, Sto) =
      (store.lookup(addr).getOrElse(Storable.V(lattice.bottom)), sto.readDep(addr))

    override def asState(conf: Conf, system: System): State =
      SchemeState(conf.c, EffectSto(Set(), Set()), conf.k, conf.t, conf.extra)

    override def asConf(s: State, system: System): Conf =
      SchemeConf(s.c, s.k, s.t, s.extra)

    override def injectConf(e: Expr): Conf =
      SchemeConf(Control.Ev(e, initialEnv), Kont0Addr, initialTime, emptyExt)

    override def inject(expr: Expr): System =
        val sys = EffectDrivenAnalysisSystem()
        sys.spawn(injectConf(expr))
        sys
