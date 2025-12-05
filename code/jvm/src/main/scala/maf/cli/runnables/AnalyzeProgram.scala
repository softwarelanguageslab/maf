package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.SchemeAnalyses
import maf.core.{Identifier, Monad}
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.language.taint.TaintSchemeParser
import maf.modular.*
import maf.modular.incremental.ProgramVersionExtracter.*
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.taint.GlobalStoreTaint
import maf.modular.taint.scheme.SchemeModFBigStepTaintSemantics
import maf.modular.worklist.*
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}
import maf.modular.scheme.modf._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.language.scheme._

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeProgram extends App:

    enum AnalysisResult:
        case Terminated(iterationCount: Int, time: Long)
        case TimedOut

    // timeout can be set e.g., using Timeout.start(Duration(1, MINUTES))
    def analyze(file: String, timeout: Timeout.T = Timeout.none): AnalysisResult = 
        val text = Reader.loadFile(file)
        val prog = SchemeParser.parseProgram(text)
        var iterations = 0
        val analysis = 
            new SimpleSchemeModFAnalysis(prog)
                with SchemeModFNoSensitivity
                with SchemeConstantPropagationDomain
                with PriorityQueueWorklistAlgorithm[SchemeExp]:
                    /** Example implementation of priority **/ 
                    lazy val ordering = Ordering.by(priority) // currently, ordering is based on calculated priority
                                                              // alternatively, could also just implement `compare` here
                    def priority(cmp: Component): Int = 
                        cmp match
                            case Main => 0  // "main" (= top-level program code) gets priority 0
                            case Call((lam, env), ctx) => priority(lam) // TODO: could also take lexical environment + context into account
                    def priority(lam: SchemeLambdaExp): Int = 
                        42   // TODO: determine the priority for each lambda in the program
                    /** Analysis setup **/
                    override def intraAnalysis(cmp: SchemeModFComponent) =
                        new IntraAnalysis(cmp) with BigStepModFIntra
                    override def step(timeout: Timeout.T) =
                        iterations += 1
                        super.step(timeout)
        val time = Timer.timeOnly { analysis.analyzeWithTimeout(timeout) } 
        if analysis.finished then
            AnalysisResult.Terminated(iterations, time/1000000)
        else 
            AnalysisResult.TimedOut

    analyze("test/R5RS/gabriel/boyer.scm") match 
        case AnalysisResult.Terminated(iterations, timeMs) =>
            println("*** Analysis terminated ***")
            println(s"-> iterations: $iterations")
            println(s"-> time: ${timeMs}ms")
        case AnalysisResult.TimedOut =>
            println("*** Analysis timed out ***")