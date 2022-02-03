package maf.cli.experiments.performance

import maf.cli.experiments.SchemeAnalyses
import maf.language.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser}
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.{AnalysisEntry, ModAnalysis}

object ModFLocalPerformance extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    def benchmarks =
      List(
      )
    def analyses =
      List(
        (SchemeAnalyses.modflocalAnalysisAdaptiveA(_, 0, 10), "0-CFA DSS"),
        (SchemeAnalyses.modflocalAnalysisAdaptiveA(_, 1, 10), "1-CFA DSS"),
        (SchemeAnalyses.kCFAAnalysis(_, 0), "0-CFA MODF"),
        (SchemeAnalyses.kCFAAnalysis(_, 1), "1-CFA MODF"),
      )
    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)
    def main(args: Array[String]): Unit =
      run("benchOutput/performance/modflocal-results.csv")
