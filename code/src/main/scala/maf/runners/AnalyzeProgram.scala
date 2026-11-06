package maf.runners

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.*
import maf.semantics.scheme.*
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.SchemeMutableVarBoxer
import maf.util.{Timeout, Timer}

object AnalyzeProgram extends App:

    enum AnalysisResult:
        case Terminated(iterationCount: Int, time: Long)
        case TimedOut

    def parse(file: String): SchemeExp =
        val parsed = SchemeParser.parseFile(file)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    // timeout can be set e.g., using Timeout.start(Duration(1, MINUTES))
    def analyze(file: String, timeout: Timeout.T = Timeout.none): AnalysisResult =
        val prog = parse(file)
        val analysis =
            new SchemeModFLocalFS(prog)
                with SchemeModFLocalNoSensitivity
                with SchemeConstantPropagationDomain
                with PriorityQueueWorklistAlgorithm[SchemeExp]:
                    /** Example implementation of priority **/
                    lazy val ordering = Ordering.by(priority) // currently, ordering is based on calculated priority
                                                              // alternatively, could also just implement `compare` here
                    def priority(cmp: Component): Int =
                        cmp match
                            case MainComponent => 0  // "main" (= top-level program code) gets priority 0
                            case CallComponent(lam, env, ctx) => priority(lam) // TODO: could also take lexical environment + context into account
                    def priority(lam: SchemeLambdaExp): Int =
                        42   // TODO: determine the priority for each lambda in the program
        val time = Timer.timeOnly { analysis.analyzeWithTimeout(timeout) }
        if analysis.finished then
            AnalysisResult.Terminated(analysis.iterations, time/1000000)
        else
            AnalysisResult.TimedOut

    analyze("test/R5RS/gabriel/boyer.scm") match
        case AnalysisResult.Terminated(iterations, timeMs) =>
            println("*** Analysis terminated ***")
            println(s"-> iterations: $iterations")
            println(s"-> time: ${timeMs}ms")
        case AnalysisResult.TimedOut =>
            println("*** Analysis timed out ***")
