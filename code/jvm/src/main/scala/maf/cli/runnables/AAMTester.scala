package maf.cli.runnables

import maf.aam.{BaseSimpleWorklistSystem, GraphElementAAM, SimpleWorklistSystem}
import maf.aam.scheme.*
import maf.language.scheme.*
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.util.*
import maf.util.graph.*
import maf.util.benchmarks.Timeout
import scala.concurrent.duration._
import scala.io.StdIn
import maf.util.benchmarks.Timer
import maf.aam.scheme.stores.SchemeImperativeStoreWidening

/** Base trait that provides analysis functionality to analyze single programs */
trait AAMTesterT:
    type Analysis <: BaseSchemeAAMSemantics
    protected def analysis(b: SchemeExp): Analysis
    protected def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    /**
     * Run the program
     * @param name
     *   the name of the program to analyze
     */
    def run(name: String): Unit =
        val content = Reader.loadFile(name)
        val program = parseProgram(content)
        val graph = new DotGraph[GraphElementAAM, GraphElement]()
        given gInst: Graph[graph.G, GraphElementAAM, GraphElement] = graph.G.typeclass
        println(s"input program ${program.prettyString(0)}")
        val theAnalysis = analysis(program)

        def findConf[T](states: Set[T], g: graph.G, id: String): Option[T] =
            val stateId = id.toIntOption
            if stateId.isEmpty then None
            else
                val state = gInst.findNodeById(g, stateId.get)
                println(s"state $state ${state.map(_.hsh)}")
                state match
                    case Some(state) =>
                      states.find(state.hsh == _.hashCode)
                    case None => None

        val (time, analysisResult) = Timer.time {
          theAnalysis.analyzeWithTimeout(Timeout.start(Duration(60, SECONDS)), graph.G.typeclass.empty)
        }

        val states = analysisResult.allConfs
        val g = analysisResult.dependencyGraph

        g.toFile(name.replace("/", "_").nn + ".dot")
        if theAnalysis.finished then
            println(s"Analysis finished in ${time / (1000 * 1000)} milliseconds, by visiting ${states.size} states")
            println(s"Set of answers ${analysisResult.values}")
        else println(s"The analysis timed-out in ${time / (1000 * 1000)} millisconds")

        print("query>>> ")
        var input = StdIn.readLine()

        while (input != ":q") do
            println(input.nn.split('.').nn.mkString("::"))
            val parts = input.split('.').nn.toList.flatMap(s => findConf(states, g, s.nn))
            for window <- parts.sliding(2, 1) do
                println("== New comparison ==")
                println(s"window $window")
                window.foreach(state => theAnalysis.printDebug(state, false))
                if window.size == 2 then theAnalysis.compareStates(window(0), window(1))

            print("query>>> ")
            input = StdIn.readLine()

object AAMTester extends AAMTesterT:
    type Analysis = BaseSchemeAAMSemantics

    class SimpleAnalysis(b: SchemeExp)
        extends BaseSchemeAAMSemantics(b)
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeAAMLocalStore
        //with BaseSchemeLoggingLocalStore
        //with SchemeImperativeStoreWidening
        //with maf.aam.scheme.stores.BaseSchemeDependencyLoggingStore
        with SchemeStoreAllocateReturn
        //with SchemeWideningAfterCondition
        with SchemeFunctionCallBoundary
        //with SchemeFunctionModularAAM
        with SimpleWorklistSystem[SchemeExp]
        //with SchemeAtomicEvaluation
        with SchemeAAMAnalysisResults {
      //override type System = LoggingLocalStoreSystem
    }

    protected def analysis(b: SchemeExp): Analysis = SimpleAnalysis(b)

    def main(args: Array[String]): Unit =
      if args.size > 0 then run(args(0)) else println("Please provide a file")
