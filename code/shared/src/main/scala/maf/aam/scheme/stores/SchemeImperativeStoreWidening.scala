package maf.aam.scheme.stores

import maf.core.*
import scala.annotation.static
import maf.language.scheme.*
import maf.aam.{AAMAnalysis, AAMGraph, GraphElementAAM}
import maf.aam.scheme.BaseSchemeAAMSemantics
import maf.util.Trampoline.run
import scala.collection.mutable.Queue
import maf.aam.scheme.AAMPeformanceMetrics
import maf.util.graph.NoTransition
import maf.util.graph.GraphElement
import maf.util.graph.Colors
import maf.util.graph.GraphMetadata
import maf.util.graph.GraphMetadataNone

// Graph visualisation edges

object Deps:
    private var ctr: Int = 0
    def genId(): Int = { ctr = ctr + 1; ctr }

object WriteDep:
    def apply(v: String): WriteDep = WriteDep(v, Deps.genId())

case class WriteDep(v: String, ctr: Int) extends GraphElement:
    def label = s"w($v, $ctr)"
    def color = Colors.DarkBlue
    def metadata: GraphMetadata = GraphMetadataNone

case class ReadDep() extends GraphElement:
    def label = s"r"
    def color = Colors.Red
    def metadata = GraphMetadataNone

def addrNode(adr: Address): GraphElementAAM =
  GraphElementAAM(adr.hashCode, adr.toString, Colors.Grass, "")

/** An effect driven imperative store */
trait SchemeImperativeStoreWidening extends AAMAnalysis[SchemeExp], BaseSchemeAAMSemantics, AAMPeformanceMetrics[SchemeExp]:
    case class SchemeConf(c: Control, k: KonA, t: Timestamp, extra: Ext, tt: Int)
    type Conf = SchemeConf
    type Sto = EffectSto

    protected val enableGraph: Boolean = false

    override type System = EffectDrivenAnalysisSystem

    private var originalSto = BasicStore(initialBds.map(p => (p._2, p._3)).toMap).extend(Kont0Addr, Storable.K(Set(HltFrame)))
    var store: BasicStore[Address, Storable] = originalSto

    override def asGraphElement(c: Conf, sys: System): GraphElementAAM =
      asGraphElement(c.c, c.k, EffectSto(Set(), Set(), c.tt), c.extra, c.hashCode)

    override lazy val initialStore: Sto = EffectSto(Set(), Set(), 0)

    /**
     * A store that collects all the effects of analyzing a single state
     *
     * @param w
     *   the write effects of the state, trigger re-analysis of states that have registered a read effect
     * @param r
     *   the read effects of the state, are registered after the analysis of a particular state
     * @param c
     *   the call effects of the state
     */
    case class EffectSto(w: Set[Address], r: Set[Address], tt: Int, c: Set[State] = Set()):
        def writeDep(a: Address): EffectSto =
          this.copy(w = w + a)

        def callDep(a: Set[State]): EffectSto =
          this.copy(c = c ++ a)

        def readDep(a: Address): EffectSto =
          this.copy(r = r + a)

        override def toString: String =
          s"EffectSto { w = ${w.size}, r = ${r.size}, c = ${c.size})"

    class EffectDrivenAnalysisSystem extends BaseSystem:
        def allConfs: Set[Conf] = seen

        def finalStates: Set[State] =
          seen.map(asState(_, this)).filter(isFinal)

        private var seen: Set[Conf] = Set()
        private var R: Map[Address, Set[Conf]] = Map().withDefaultValue(Set())
        private val worklist: Queue[(Option[Conf], Conf)] = Queue()

        def popWork(): Option[(Option[Conf], Conf)] =
          if worklist.nonEmpty then
              change {
                val conf = worklist.dequeue
                Some(conf)
              }
          else None

        /** Trigger some read dependencies */
        def trigger(w: Set[Address], from: Conf): Unit =
          change {
            w.foreach(adr => R(adr).foreach(conf => worklist.enqueue((Some(from), conf.copy(tt = conf.tt)))))
          }

        /** Register a read dependency */
        def register(r: Set[Address], conf: Conf): Unit =
          change {
            r.foreach(adr => R = R + (adr -> (R(adr) + conf)))
          }

        /** Spawn a configuration */
        def spawn(conf: Conf, from: Conf): Boolean =
          spawn(conf, Some(from))

        def spawn(conf: Conf, from: Option[Conf]): Boolean =
            if !seen.contains(conf) then
                increment(Seen)
                change {
                  seen = seen + conf
                  worklist.enqueue((from, conf))
                }
                true
            else increment(Bump); false

    override def writeSto(sto: Sto, a: Address, value: Storable): Sto =
        // directly write it to the global store
        val oldValue = store.lookup(a)
        store = store.extend(a, value)
        if oldValue == store.lookup(a) then sto else sto.writeDep(a)

    override protected def transition[G](system: System, dependencyGraph: G)(using g: AAMGraph[G]): (System, G) =
        val workOption = system.popWork()
        if workOption.isEmpty then (system, dependencyGraph)
        else
            val (from, work) = workOption.get
            var graph = dependencyGraph

            val successors = run(step(asState(work, system)))
            successors.foreach { next =>
                val sto = next.s
                val nextConf = asConf(next, system)

                ///////////////////// GRAPH ///////////////////////////:
                if enableGraph then
                    sto.r.foreach(adr => graph = g.addEdge(graph, addrNode(adr), ReadDep(), asGraphElement(work, system)))
                    sto.w.foreach(adr =>
                        val vlu = this.store.lookup(adr).getOrElse(lattice.bottom)
                        val writeDep = WriteDep(vlu match
                            case Storable.V(v) => v.toString
                            case _             => ""
                        )
                        graph = g.addEdge(graph, asGraphElement(work, system), writeDep, addrNode(adr))
                    )

                /////////////////////// ModF //////////////////////////////

                // Spawn new components
                (sto.c.map(asConf(_, system)) + nextConf).foreach(system.spawn(_, work))

                // Register read dependencies
                system.register(sto.r, work)

                // Trigger read dependencies using write effects
                system.trigger(sto.w, work)
            }
            (system, graph)

    override def readSto(sto: Sto, addr: Address): (Storable, Sto) =
        val vlu = store.lookup(addr).getOrElse(Storable.V(lattice.bottom))
        (vlu, sto.readDep(addr))

    override def asState(conf: Conf, system: System): State =
      SchemeState(conf.c, EffectSto(Set(), Set(), conf.tt), conf.k, conf.t, conf.extra)

    override def asConf(s: State, system: System): Conf =
      SchemeConf(s.c, s.k, s.t, s.extra, s.s.tt)

    override def injectConf(e: Expr): Conf =
      SchemeConf(Control.Ev(e, initialEnv), Kont0Addr, initialTime, emptyExt, 0)

    override def inject(expr: Expr): System =
        val sys = EffectDrivenAnalysisSystem()
        sys.spawn(injectConf(expr), None)
        sys
