package maf.test.modular.scheme.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular._
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental.ProgramVersionExtracter._
import maf.modular.incremental._
import maf.modular.incremental.scheme.lattice.IncrementalSchemeConstantPropagationDomain
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf._
import maf.modular.scheme.ssmodconc._
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test._
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag

import scala.concurrent.duration._

/** Tests comparing the initial result of an analysis to the result of an incremental analysis. */
// TODO: Also factor out onBenchmark.
trait IncrementalModXComparisonTests extends SchemeBenchmarkTests {
  lazy val configurations: List[IncrementalConfiguration] = allConfigurations
  def timeout(): Timeout.T = Timeout.start(Duration(5, MINUTES))

}

class ModFComparisonTests extends IncrementalModXComparisonTests with SequentialIncrementalBenchmarks {

  abstract class BaseAnalysis(program: SchemeExp)
      extends ModAnalysis[SchemeExp](program)
         with StandardSchemeModFComponents
         with SchemeModFNoSensitivity
         with SchemeModFSemantics
         with IncrementalSchemeConstantPropagationDomain // Use the incremental domain for both.
         with LIFOWorklistAlgorithm[SchemeExp]

  class IncrementalAnalysis(program: SchemeExp)
      extends BaseAnalysis(program)
         with IncrementalSchemeModFBigStepSemantics
         with IncrementalGlobalStore[SchemeExp] {
    var configuration: IncrementalConfiguration = allOptimisations
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
  }

  class Analysis(program: SchemeExp) extends BaseAnalysis(program) with BigStepModFSemantics with GlobalStore[SchemeExp] {
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with BigStepModFIntra with GlobalStoreIntra
  }

  def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag] =
    if (c == allOptimisations || c == noOptimisations) Seq(IncrementalTest) else Seq(IncrementalTest, SlowTest)

  // TODO: Better comparisons, remove typecasts.
  def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size == a.visited.size)
    assert(i.store.size == a.store.size)
    assert(i.visited.diff(a.visited).isEmpty) // If the size is equal, this checks also the converse assertion.
    assert(a.store.forall { case (ad, v) =>
      v == i.store.getOrElse(ad, i.lattice.bottom)
    }) // No need to remove annotations (should not be in the store).
  }

  // TODO: Better comparisons, remove typecasts.
  def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size >= a.visited.size)
    assert(i.store.size >= a.store.size)
    assert(a.visited.diff(i.visited).isEmpty) // If the size is equal, this checks also the converse assertion.
    assert(a.store.forall { case (ad, v) => a.lattice.subsumes(i.store.getOrElse(ad, a.lattice.bottom), v) })
  }

  def onBenchmark(benchmark: Benchmark): Unit = {
    val text = Reader.loadFile(benchmark)
    val program = CSchemeParser.parse(text)
    for (c <- configurations) {
      property(s"Analysis results for $benchmark using $c are equal (initial analysis) or subsuming (incremental update).",
               testTags(benchmark, c): _*
      ) {
        try {
          val a = new Analysis(getInitial(program))
          a.analyzeWithTimeout(timeout())
          assume(a.finished)
          val i = new IncrementalAnalysis(program)
          i.configuration = c
          i.analyzeWithTimeout(timeout())
          assume(i.finished)
          checkEqual(a, i)

          ////

          val b = new Analysis(getUpdated(program))
          b.analyzeWithTimeout(timeout())
          assume(b.finished)
          i.updateAnalysis(timeout())
          assume(i.finished)
          checkSubsumption(a, i)

        } catch {
          case e: VirtualMachineError =>
            System.gc()
            cancel(s"Analysis of $benchmark encountered an error: $e.")
        }
      }
    }
  }
}

class ModConcComparisonTests extends IncrementalModXComparisonTests with ConcurrentIncrementalBenchmarks {
  abstract class BaseModConcAnalysis(prg: SchemeExp)
      extends ModAnalysis[SchemeExp](prg)
         with KKallocModConc
         with LIFOWorklistAlgorithm[SchemeExp]
         with IncrementalSchemeConstantPropagationDomain {
    val k: Int = 1
  }

  class IncrementalAnalysis(program: SchemeExp)
      extends BaseModConcAnalysis(program)
         with IncrementalSchemeModConcSmallStepSemantics
         with IncrementalGlobalStore[SchemeExp] {
    var configuration: IncrementalConfiguration = allOptimisations

    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis
  }

  class Analysis(program: SchemeExp) extends BaseModConcAnalysis(program) with SmallStepModConcSemantics with GlobalStore[SchemeExp] {
    override def intraAnalysis(
        cmp: SmallStepModConcComponent
      ): KCFAIntra = new IntraAnalysis(cmp) with SmallStepIntra with KCFAIntra with GlobalStoreIntra
  }

  def testTags(b: Benchmark): Seq[Tag] = Seq(IncrementalTest, SlowTest)

  // TODO: Better comparisons, remove typecasts.
  def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size == a.visited.size)
    assert(i.store.size == a.store.size)
    assert(i.visited.diff(a.visited).isEmpty) // If the size is equal, this checks also the converse assertion.
    assert(a.store.forall { case (ad, v) =>
      v == i.store.getOrElse(ad, i.lattice.bottom)
    }) // No need to remove annotations (should not be in the store).
  }

  // TODO: Better comparisons, remove typecasts.
  def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size >= a.visited.size)
    assert(i.store.size >= a.store.size)
    assert(a.visited.diff(i.visited).isEmpty) // If the size is equal, this checks also the converse assertion.
    assert(a.store.forall { case (ad, v) => a.lattice.subsumes(i.store.getOrElse(ad, a.lattice.bottom), v) })
  }

  override def onBenchmark(benchmark: Benchmark): Unit = {
    val text = Reader.loadFile(benchmark)
    val program = CSchemeParser.parse(text)
    for (c <- configurations) {
      property(s"Analysis results for $benchmark using $c are equal (initial analysis) or subsuming (incremental update).", testTags(benchmark): _*) {
        try {
          val a = new Analysis(getInitial(program))
          a.analyzeWithTimeout(timeout())
          assume(a.finished)
          val i = new IncrementalAnalysis(program)
          i.configuration = c
          i.analyzeWithTimeout(timeout())
          assume(i.finished)
          checkEqual(a, i)

          ////

          val b = new Analysis(getUpdated(program))
          b.analyzeWithTimeout(timeout())
          assume(b.finished)
          i.updateAnalysis(timeout())
          assume(i.finished)
          checkSubsumption(a, i)

        } catch {
          case e: VirtualMachineError =>
            System.gc()
            cancel(s"Analysis of $benchmark encountered an error: $e.")
        }
      }
    }
  }
}
