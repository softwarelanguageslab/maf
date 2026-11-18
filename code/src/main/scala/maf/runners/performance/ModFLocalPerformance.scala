package maf.runners.performance

import maf.runners.SchemeAnalyses
import maf.syntax.Source
import maf.syntax.scheme.{Named, SchemeExp, SchemeParser}
import maf.syntax.scheme.transformations.{Preluded, SchemeMutableVarBoxer, SchemePrelude}

object ModFLocalPerformance extends PerformanceEvaluation:
    type Analysis = maf.fixpoint.Analysis[SchemeExp[Named] & Preluded]
    def benchmarks =
        List(
        )
    def analyses =
        List(
          (SchemeAnalyses.modflocalAnalysis(0), "0-CFA DSS"),
          (SchemeAnalyses.modflocalAnalysis(1), "1-CFA DSS"),
          (SchemeAnalyses.modflocalFSAnalysis(0), "0-CFA DSS-FS"),
          (SchemeAnalyses.modflocalFSAnalysis(1), "1-CFA DSS-FS"),
        )
    override def parseProgram(txt: String, source: Source): SchemeExp[Named] & Preluded =
        val parsed = SchemeParser.parse(txt, source)
        val prelud = SchemePrelude.addPrelude(parsed)
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)
    def main(args: Array[String]): Unit =
        run("benchOutput/performance/modflocal-results.csv")
