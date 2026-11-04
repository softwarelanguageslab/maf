package maf.runners.performance

import maf.fixpoint.AnalysisEntry
import maf.runners.SchemeAnalyses
import maf.syntax.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser, SchemePrelude}

object ModFLocalPerformance extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    def benchmarks =
        List(
        )
    def analyses =
        List(
          (SchemeAnalyses.modflocalAnalysis(_, 0), "0-CFA DSS"),
          (SchemeAnalyses.modflocalAnalysis(_, 1), "1-CFA DSS"),
          (SchemeAnalyses.modflocalFSAnalysis(_, 0), "0-CFA DSS-FS"),
          (SchemeAnalyses.modflocalFSAnalysis(_, 1), "1-CFA DSS-FS"),
        )
    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)
    def main(args: Array[String]): Unit =
        run("benchOutput/performance/modflocal-results.csv")
