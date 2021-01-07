package maf.test.modular.scheme.incremental

import org.scalatest.Tag
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular._
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme._
import maf.test._
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration.{Duration, MINUTES}

/**
 * Trait implementing soundness tests for incremental analyses.<br>
 * Following properties are checked:
 * <ul>
 *   <li>The soundness of the initial analysis of the original program.</li>
 *   <li>The soundness of the incremental update of the program.</li>
 * </ul>
 * The properties are checked by comparing the analysis results against the results of a concrete interpreter (run on both program versions).
 * The comparisons are implemented in {@link SchemeSoundnessTests}.
 *
 * @see SchemeSoundnessTests
 */
trait IncrementalModXSoundnessTests extends SchemeSoundnessTests {

  type IncrementalAnalysis = ModAnalysis[SchemeExp]
    with GlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SchemeDomain
    with IncrementalModAnalysis[SchemeExp]

  override def analysis(b: SchemeExp): IncrementalAnalysis
  override def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(3, MINUTES))

  private var version: Version = Old

  override def runInterpreter(
      i: SchemeInterpreter,
      p: SchemeExp,
      t: Timeout.T
    ): SchemeInterpreter.Value = i.run(p, t, version)

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Incremental (re-)analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = CSchemeParser.parse(content)

      // Check soundness on the original version of the program.
      version = Old
      val (cResultOld, cPosResultsOld) = evalConcrete(program, benchmark)
      val anlOld = runAnalysis(program, benchmark)
      compareResult(anlOld, cResultOld)
      compareIdentities(anlOld, cPosResultsOld)

      // Check soundness on the updated version of the program.
      version = New
      val (cResultNew, cPosResultsNew) = evalConcrete(program, benchmark)
      val anlNew = updateAnalysis(program, benchmark)
      compareResult(anlNew, cResultNew)
      compareIdentities(anlNew, cPosResultsNew)
    }

  private def updateAnalysis(program: SchemeExp, benchmark: Benchmark): IncrementalAnalysis =
    try {
      val anl: IncrementalAnalysis = analysis(program)
      val timeout = analysisTimeout(benchmark)
      anl.updateAnalysis(timeout)
      assume(anl.finished(), "Reanalysis timed out.")
      anl
    } catch {
      case e: VirtualMachineError =>
        System.gc()
        cancel(s"Reanalysis of $benchmark encountered an error: $e.")
    }

  override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ IncrementalTest
}

/** Implements soundness tests for an incremental ModConc analysis. */
class IncrementalSmallStepModConc extends IncrementalModXSoundnessTests with ConcurrentIncrementalBenchmarks {
  def name = "Incremental ModConc"
  override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalModConcAnalysis(b)
  override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ SchemeModConcTest :+ SmallStepTest
  override def isSlow(b: Benchmark): Boolean =
    Set(
      "test/changes/cscheme/threads/actors.scm",
      "test/changes/cscheme/threads/crypt.scm",
      "test/changes/cscheme/threads/crypt2.scm",
      "test/changes/cscheme/threads/stm.scm"
    )(b)
}

/** Implements soundness tests for an incremental ModConc analysis. */
class IncrementalSmallStepModConcCP extends IncrementalSmallStepModConc {
  override def name = "Incremental ModConc CP"
  override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalModConcCPAnalysis(b)
}

/** Implements soundness tests for an incremental ModF analysis. */
class IncrementalModF extends IncrementalModXSoundnessTests with SequentialIncrementalBenchmarks {
  def name = "Incremental ModF"
  override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFAnalysis(b)
  override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ SchemeModFTest :+ BigStepTest
  override def isSlow(b: Benchmark): Boolean =
    Set(
      "test/changes/scheme/multiple-dwelling (coarse).scm",
      "test/changes/scheme/multiple-dwelling (fine).scm",
      "test/changes/scheme/leval.scm",
      "test/changes/scheme/machine-simulator.scm",
      "test/changes/scheme/mceval-dynamic.scm",
      "test/changes/scheme/nboyer.scm",
      "test/changes/scheme/peval.scm"
    )(b)
}

/** Implements soundness tests for an incremental ModF analysis. */
class IncrementalModFCP extends IncrementalModF {
  override def name = "Incremental ModF CP"
  override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFCPAnalysis(b)
}
