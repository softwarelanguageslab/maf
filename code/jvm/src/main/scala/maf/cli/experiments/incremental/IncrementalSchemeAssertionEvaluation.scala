package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental._
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations._
import maf.modular.scheme.modf.SchemeAssertSemantics
import maf.util.Reader
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalSchemeAssertionEvaluation extends IncrementalExperiment[SchemeExp] with TableOutput[String] {

  final val veriS: String = "veri" // Verified assertions.
  final val failS: String = "fail" // Failed assertions.

  val propertiesS = List(veriS, failS)

  var results: Table[String] = Table.empty.withDefaultValue(" ")
  val error: String = errS

  type Analysis = IncrementalModAnalysis[SchemeExp] with IncrementalGlobalStore[SchemeExp] with SchemeAssertSemantics

  def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))

  override val catchErrors: Boolean = true // Put to false for debugging.

  def runAnalysis(
      file: String,
      phase: String,
      block: Timeout.T => Analysis
    ): Boolean = {
    print(phase)
    val timeOut = timeout()
    val a = block(timeOut)
    val t = timeOut.reached
    if (t) {
      print(" timed out - ")
      results = results.add(file, columnName(veriS, phase), infS)
      results = results.add(file, columnName(failS, phase), infS)
    } else {
      print(" ")
      results = results.add(file, columnName(veriS, phase), a.assertionsVerified.size.toString)
      results = results.add(file, columnName(failS, phase), a.assertionsFailed.size.toString)
    }
    t

  }

  def onBenchmark(file: String): Unit = {
    print(s"Analysing $file: ")
    val program = parse(file)

    val a1 = analysis(program, Config(cyclicValueInvalidation = false))
    val a2 = analysis(program, noOptimisations) // This configuration does not matter.
    a2.version = New

    if (
      runAnalysis(file,
                  "init",
                  timeOut => {
                    a1.analyzeWithTimeout(timeOut);
                    a1
                  }
      )
    ) {
      println()
      return
    }

    val a1Copy = a1.deepCopy()
    a1Copy.configuration = allOptimisations

    runAnalysis(file,
                "inc1",
                timeOut => {
                  a1.updateAnalysis(timeOut);
                  a1
                }
    )
    runAnalysis(file,
                "inc2",
                timeOut => {
                  a1Copy.updateAnalysis(timeOut);
                  a1Copy
                }
    )
    runAnalysis(file,
                "rean",
                timeOut => {
                  a2.analyzeWithTimeout(timeOut);
                  a2
                }
    )
  }

  def createOutput(): String = {
    // Mark rows that are different between the two incremental versions.
    results.allRows.foreach { row =>
      if (
        results.get(row, columnName(veriS, inc1S)) != results.get(row, columnName(veriS, inc2S))
        || results.get(row, columnName(failS, inc1S)) != results.get(row, columnName(failS, inc2S))
      ) {
        results = results.add(row, "diff inc", "x")
      }
      if (
        results.get(row, columnName(veriS, reanS)) != results.get(row, columnName(veriS, inc2S))
        || results.get(row, columnName(failS, reanS)) != results.get(row, columnName(failS, inc2S))
      ) {
        results = results.add(row, "diff rean", "x")
      }
    }
    results.prettyString(columns = columns ++ Set("diff inc", "diff rean"))
  }
}

trait IncrementalSchemeModFAssertionEvaluation extends IncrementalSchemeAssertionEvaluation {
  def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.assertions ++ IncrementalSchemeBenchmarkPrograms.sequential

  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))

}

object IncrementalSchemeBigStepCPAssertionEvaluation extends IncrementalSchemeModFAssertionEvaluation {
  override def analysis(e: SchemeExp, config: IncrementalConfiguration) = new IncrementalSchemeModFAssertionAnalysisCPLattice(e, config)

  override val outputFile: String = s"assertions/modf-CP.txt"
}

object IncrementalSchemeBigStepTypeAssertionEvaluation extends IncrementalSchemeModFAssertionEvaluation {
  override def analysis(e: SchemeExp, config: IncrementalConfiguration) = new IncrementalSchemeModFAssertionAnalysisTypeLattice(e, config)

  override val outputFile: String = s"assertions/modf-Type.txt"
}
