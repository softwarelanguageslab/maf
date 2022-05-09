package maf.cli.runnables.scv

import maf.modular.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.worklist.*
import maf.language.scheme.*
import maf.language.symbolic.lattices.*
import maf.language.scheme.lattices.*
import maf.language.ContractScheme.*
import maf.modular.scv.*
import maf.lattice.*
import maf.core.{Address, Identity}
import maf.cli.modular.scv.*
import scala.reflect.ClassTag
import maf.util.graph.*
import maf.util.graph.Graph.GraphOps
import maf.util.Reader

trait TrackTriggeredDependencies extends ModAnalysis[SchemeExp]:
    var triggeredDeps: Map[Component, Set[Dependency]] = Map().withDefaultValue(Set())
    var calledDeps: Map[Component, Set[Component]] = Map().withDefaultValue(Set())

    override def intraAnalysis(component: Component): IntraTrackAnalysis

    trait IntraTrackAnalysis extends IntraAnalysis:
        override def trigger(dep: Dependency): Unit =
            triggeredDeps = triggeredDeps + (component -> (triggeredDeps(component) + dep))
            super.trigger(dep)

        override def spawn(calledCmp: Component): Unit =
            calledDeps = calledDeps + (component -> (calledDeps(component) + calledCmp))
            super.spawn(calledCmp)

object ScvGraphGenerator:
    protected val INCLUDE_PRIMITIVES = false

    case class Node(label: String, color: Color) extends maf.util.graph.GraphElement:
        def metadata: GraphMetadata = GraphMetadataNone

    sealed trait Edge extends GraphElement:
        def color: Color = Colors.Black
        def metadata: GraphMetadata = GraphMetadataNone
    case object WriteDep extends Edge:
        val label: String = "w"
    case object ReadDep extends Edge:
        val label: String = "r"
    case object CallDep extends Edge:
        val label: String = "c"
    case object SymDep extends Edge:
        val label: String = ""

    def parseProgram(txt: String): SchemeExp =
        SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)

    def analysis(prg: SchemeExp): ScvModAnalysis with TrackTriggeredDependencies with FunctionSummaryAnalysis =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
            with ScvBigStepSemantics
            with SymbolicSchemeConstantPropagationDomain
            with StandardSchemeModFComponents
            with LIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFSemanticsM
            with ScvOneContextSensitivity(0)
            with ScvBigStepWithProvides
            with ScvWithStructs
            with ScvIgnoreFreshBlame
            //with maf.modular.scv.ScvFullPathSensitivity
            with TrackTriggeredDependencies
            with maf.modular.scv.FunctionSummaryAnalysis:
            // with UnstableWideningWithMinimum(2)
            // with RemovePathCondition:
            //with UnstableWideningWithMinimum(2):
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp)
                with IntraScvSemanticsWithProvides
                with IntraScvSemanticsWithStructs
                with IntraScvIgnoreFreshBlames
                with IntraTrackAnalysis
                with FunctionSummaryIntra
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver(this)

    def buildGraph(filename: String, anl: TrackTriggeredDependencies with FunctionSummaryAnalysis): Unit =
        implicit val graph = new DotGraph[Node, Edge].G.typeclass
        var g = graph.empty
        // add all the nodes once to the graph
        g = anl.visited.foldLeft(g)((g, cmp) => g.addNode(Node(cmp.toString, Colors.Red)))
        // add all the read dependencies
        g = anl.deps.foldLeft(g) { case (g, (dep, cmps)) =>
            cmps.foldLeft(g)((g, cmp) => g.addEdge(Node(dep.toString, Colors.Blue), ReadDep, Node(cmp.toString, Colors.Red)))
        }
        // add all the write dependencies
        g = anl.triggeredDeps.foldLeft(g) { case (g, (cmp, deps)) =>
            deps.foldLeft(g)((g, dep) => g.addEdge(Node(cmp.toString, Colors.Red), WriteDep, Node(dep.toString, Colors.Blue)))
        }
        // add all call dependencies to the graph
        g = anl.calledDeps.foldLeft(g) { case (g, (cmp, calledCmps)) =>
            calledCmps.foldLeft(g)((g, calledCmp) => g.addEdge(Node(cmp.toString, Colors.Red), CallDep, Node(calledCmp.toString, Colors.Red)))
        }
        // add all dependencies of symbolic representations to addresses in the graph
        g = anl.functionSummaries.foldLeft(g) {
            case (g, (cmp, Some(summary))) =>
                val g1 = summary.addresses.foldLeft(g) { case (g, (sym, addrs)) =>
                    addrs.foldLeft(g)((g, addr) => g.addEdge(Node(s"$cmp:$sym", Colors.Green), SymDep, Node(AddrDependency(addr).toString, Colors.Blue)))
                }
                // also add the owned symbols
                summary.vars.foldLeft(g1) { case (g, sym) =>
                    g.addEdge(Node(s"$cmp:$sym", Colors.Green), SymDep, Node(cmp.toString, Colors.Blue))
                }
            case _ => g
        }
        // now write it out to a file
        val graphFilename = filename.replace("/", "_").nn + ".dot"
        g.toFile(s"out/$graphFilename")

    def main(args: Array[String]): Unit =
        if (args.size < 1) then println(s"Invalid number of arguments, got ${args.size} but expected 1")
        else
            val filename = args(0)
            val txt = Reader.loadFile(filename)
            val program = parseProgram(txt)
            val anl = analysis(program)
            anl.analyzeWithTimeoutInSeconds(30)
            buildGraph(filename, anl)
