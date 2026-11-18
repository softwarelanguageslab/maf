package maf.runners

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.{Metric, analyze}
import maf.fixpoint.worklist.FIFOWorklistAlgorithm
import maf.semantics.scheme.*
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.{Preluded, SchemeMutableVarBoxer, SchemePrelude}
import maf.util.{Timeout, Timer}

object AnalyzeProgram extends App:

    enum AnalysisResult:
        case Terminated(iterationCount: Int, time: Long)
        case TimedOut

    def parse(file: String): SchemeExp[Named] & Preluded =
        val parsed = SchemeParser.parseFile(file)
        val prelud = SchemePrelude.addPrelude(parsed)
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    // timeout can be set e.g., using Timeout.start(Duration(1, MINUTES))
    def analyze(file: String, timeout: Timeout.T = Timeout.none): AnalysisResult =
        val prog = parse(file)
        val analysis =
            new SchemeModFLocalFS
                with SchemeModFLocalNoSensitivity
                with SchemeConstantPropagationDomain
        val (time, solver) = Timer.time { analysis.analyze(prog, FIFOWorklistAlgorithm, timeout) }
        if solver.finished then
            val iterations = solver.metrics.collectFirst { case Metric("iterations", n) => n.toInt }.get
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
