package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf.SchemeAssertSemantics
import maf.util.Reader
import maf.util.benchmarks.*

import scala.concurrent.duration.*

trait IncrementalSchemeAssertionEvaluation extends IncrementalExperiment[SchemeExp] with TableOutput[String]:

    final val veriS: String = "veri" // Verified assertions.
    final val failS: String = "fail" // Failed assertions.

    val propertiesS = List(veriS, failS)

    var results: Table[String] = Table.empty.withDefaultValue(" ")
    val error: String = errS

    type Analysis = IncrementalModAnalysis[SchemeExp]
      with IncrementalGlobalStore[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with SchemeAssertSemantics

    def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))

    override val catchErrors: Boolean = true // Put to false for debugging.

    def runAnalysis(
        file: String,
        phase: String,
        block: Timeout.T => Analysis
      ): Boolean =
        print(phase)
        val timeOut = timeout()
        val a = block(timeOut)
        val t = timeOut.reached
        if t then
            print(" timed out - ")
            results = results.add(file, columnName(veriS, phase), infS)
            results = results.add(file, columnName(failS, phase), infS)
        else
            print(" ")
            results = results.add(file, columnName(veriS, phase), a.assertionsVerified.size.toString)
            results = results.add(file, columnName(failS, phase), a.assertionsFailed.size.toString)
        t

    def onBenchmark(file: String): Unit =
        print(s"Analysing $file: ")
        val program = parse(file)

        val a1 = analysis(program, IncrementalConfiguration(cyclicValueInvalidation = false))
        val a2 = analysis(program, noOptimisations) // This configuration does not matter.
        a2.version = New

        if runAnalysis(file,
                       "init",
                       timeOut => {
                         a1.analyzeWithTimeout(timeOut);
                         a1
                       }
            )
        then
            println()
            return

        configurations.foreach { config =>
            val copy = a1.deepCopy()
            copy.configuration = config
            runAnalysis(file,
                        config.toString,
                        timeOut => {
                          copy.updateAnalysis(timeOut);
                          copy
                        }
            )
        }

        runAnalysis(file,
                    "rean",
                    timeOut => {
                      a2.analyzeWithTimeout(timeOut);
                      a2
                    }
        )

    def createOutput(): String = results.prettyString() /*{
    // Mark rows that are different between the incremental versions or that differ from the reanalysis.
    results.allRows.foreach { row =>
      val veriL = configurations.map(c => results.get(row, columnName(veriS, c.shortName())))
      val failL = configurations.map(c => results.get(row, columnName(failS, c.shortName())))
      val reanV = results.get(row, columnName(veriS, reanS))
      val reanF = results.get(row, columnName(failS, reanS))
      if (veriL.exists(_ != veriL.head) || failL.exists(_ != failL.head)) {
        results = results.add(row, "diff inc", "x")
      }
      if (veriL.exists(_ != reanV) || failL.exists(_ != reanF)) {
        results = results.add(row, "diff rean", "x")
      }
    }
    results.prettyString(columns = columns ++ Set("diff inc", "diff rean"))
  }*/

trait IncrementalSchemeModFAssertionEvaluation extends IncrementalSchemeAssertionEvaluation:
    def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.assertions ++ IncrementalSchemeBenchmarkPrograms.sequential
    override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
    val configurations: List[IncrementalConfiguration] = allConfigurations

object IncrementalSchemeBigStepCPAssertionEvaluation extends IncrementalSchemeModFAssertionEvaluation:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration) = new IncrementalSchemeModFAssertionAnalysisCPLattice(e, config)
    override val outputFile: String = s"assertions/modf-CP.txt"

object IncrementalSchemeBigStepTypeAssertionEvaluation extends IncrementalSchemeModFAssertionEvaluation:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration) = new IncrementalSchemeModFAssertionAnalysisTypeLattice(e, config)
    override val outputFile: String = s"assertions/modf-Type.txt"
