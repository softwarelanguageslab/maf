package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.modular.GlobalStore
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.incremental.scheme.SchemeAnalyses.IncrementalSchemeModFAssertionAnalysisCPLattice
import maf.modular.scheme.modf.SchemeAssertSemantics
import maf.util.Reader
import maf.util.benchmarks._

import scala.concurrent.duration._

// Note that the sets of verified/failed assertions grow monotonically.
trait IncrementalSchemeAssertionEvaluation extends IncrementalExperiment[SchemeExp] {

  final val initS: String = "init" // Initial run.
  final val inc1S: String = "inc1" // Incremental update.
  final val inc2S: String = "inc2" // Another incremental update (same changes, different analysis).
  final val reanS: String = "rean" // Full reanalysis.

  final val veriS: String = "veri" // Verified assertions.
  final val failS: String = "fail" // Failed assertions.

  val rs = List(veriS, failS)
  val columns = List(initS, inc1S, inc2S, reanS).flatMap(a => rs.map(r => s"$r ($a)"))

  final val inf: String = "∞"
  final val err: String = "E"

  var results: Table[String] = Table.empty.withDefaultValue(" ")

  type A = IncrementalModAnalysis[SchemeExp] with GlobalStore[SchemeExp] with SchemeAssertSemantics

  override def analysis(e: SchemeExp): A

  def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))

  def runAnalysis(
      file: String,
      phase: String,
      block: Timeout.T => A
    ): Boolean = {
    print(phase)
    val timeOut = timeout()
    val a = block(timeOut)
    val t = timeOut.reached
    if (t) {
      print(" timed out - ")
      results = results.add(file, s"$veriS ($phase)", inf)
      results = results.add(file, s"$failS ($phase)", inf)
    } else {
      print(" ")
      results = results.add(file, s"$veriS ($phase)", a.assertionsVerified.size.toString)
      results = results.add(file, s"$failS ($phase)", a.assertionsFailed.size.toString)
    }
    t

  }

  def onBenchmark(file: String): Unit = {
    print(s"Analysing $file: ")
    val program = parse(file)

    val a1 = analysis(program)
    val a2 = analysis(program)
    a2.version = New

    if (
      runAnalysis(file,
                  "init",
                  timeOut => {
                    a1.analyze(timeOut);
                    a1
                  }
      )
    ) {
      println()
      return
    }

    val a1Copy = a1.deepCopy()

    runAnalysis(file,
                "inc1",
                timeOut => {
                  a1.updateAnalysis(timeOut, false);
                  a1
                }
    )
    runAnalysis(file,
                "inc2",
                timeOut => {
                  a1Copy.updateAnalysis(timeOut, true);
                  a1Copy
                }
    )
    runAnalysis(file,
                "rean",
                timeOut => {
                  a2.analyze(timeOut);
                  a2
                }
    )
  }

  def reportError(file: String): Unit = columns.foreach(c => results = results.add(file, c, err))

  def createOutput(): String = results.prettyString(columns = columns)
}

object IncrementalSchemeBigStepCPAssertionEvaluation extends IncrementalSchemeAssertionEvaluation {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.assertions ++ IncrementalSchemeBenchmarkPrograms.sequential

  override def analysis(e: SchemeExp) = new IncrementalSchemeModFAssertionAnalysisCPLattice(e)

  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))

  override val outputFile: String = s"assertions/modf-CP.txt"
}
