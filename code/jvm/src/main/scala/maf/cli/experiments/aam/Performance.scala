package maf.cli.experiments.aam

import maf.cli.experiments.performance.*
import maf.aam.*
import maf.language.scheme.*
import maf.language.ContractScheme.*
import maf.core.*
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.benchmarks.*
import maf.modular.*
import maf.util.graph.*
import maf.cli.experiments.SchemeAnalyses

enum AllAnalyisTypes:
    case ModF(analysis: ModAnalysis[SchemeExp])
    case AAM(analysis: AAMAnalysis)

object AllAnalyisTypes:
    given AnalysisIsFinished[AllAnalyisTypes] with
        type T = AllAnalyisTypes
        def isFinished(analysis: T): Boolean =
          analysis match
              case ModF(anl) => anl.finished
              case AAM(anl)  => anl.finished

        def doAnalyzeWithTimeout(analysis: T, timeout: Timeout.T): Any =
          analysis match
              case ModF(anl) => anl.analyzeWithTimeout(timeout)
              case AAM(anl) =>
                val g = new NoGraph[GraphElementAAM, GraphElement]
                anl.analyzeWithTimeout(timeout, g.G())(using g.G.typeclass)

/** Compare the performance of AAM with a ModF style analysis */
trait AAMPerformanceComparison extends PerformanceEvaluation:
    type Analysis = AllAnalyisTypes

    protected def wrap(f: SchemeExp => AAMAnalysis): SchemeExp => Analysis = (exp) => AllAnalyisTypes.AAM(f(exp))
    protected def wrapModF(f: SchemeExp => ModAnalysis[SchemeExp]): SchemeExp => Analysis = (exp) => AllAnalyisTypes.ModF(f(exp))

object AAMModFPerformanceComparison extends AAMPerformanceComparison:
    def benchmarks = SchemeBenchmarkPrograms.fromFolder("test/R5RS/various")(
      ".DS_Store",
      "loop2.scm", // weirdly seems to be stuck for classic AAM
      "grid.scm", // timeout even with function boundaries
      "pico.scm", // weird errors about continuations
      "regex.scm", // time-out? why?
    )

    def analyses: List[(SchemeExp => Analysis, String)] =
      List(
        //(wrap(AAMAnalyses.aamBase), "aamBase"),
        (wrap(AAMAnalyses.aamBaseFnBoundaries), "aamFn"),
        (wrapModF(SchemeAnalyses.kCFAAnalysis(_, 0)), "0cfaModf")
      )
    def main(args: Array[String]): Unit =
      run(timeoutFast = false)

object ScvPerformanceComparison extends AAMPerformanceComparison:
    override def parseProgram(txt: String): SchemeExp =
      SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)

    def benchmarks = SchemeBenchmarkPrograms.fromFolder("test/scv/manual/safe")(".DS_Store")

    def analyses: List[(SchemeExp => Analysis, String)] =
      List((wrap(AAMAnalyses.scvAAMbase), "scvAAMbase"), (wrap(AAMAnalyses.scvAAMFnCallBoundaries), "scvAAMFfn"))

    def main(args: Array[String]): Unit =
      run(timeoutFast = false)
