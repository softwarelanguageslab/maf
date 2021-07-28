package maf.test.modular.scheme.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular._
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental._
import maf.modular.incremental.scheme.lattice.IncrementalSchemeConstantPropagationDomain
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf.EvalM.EvalM
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

  def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag]

  type Analysis <: ModAnalysis[SchemeExp]
  type IncrementalAnalysis <: IncrementalModAnalysis[SchemeExp]

  // The original program also needs a program version, as otherwise the programs of the analyses differ and components and some values cannot be compared.
  def analysis(e: SchemeExp, version: Version): Analysis
  def incAnalysis(e: SchemeExp): IncrementalAnalysis

  def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit
  def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit

  def onBenchmark(benchmark: Benchmark): Unit = {
    val text = Reader.loadFile(benchmark)
    val program = CSchemeParser.parse(text)
    for (c <- configurations) {
      property(s"Analysis results for $benchmark using $c are equal (initial analysis) or subsuming (incremental update).",
               testTags(benchmark, c): _*
      ) {
        try {
          val a = analysis(program, Old)
          a.analyzeWithTimeout(timeout())
          assume(a.finished)
          val i = incAnalysis(program)
          i.configuration = c
          i.analyzeWithTimeout(timeout())
          assume(i.finished)
          checkEqual(a, i)

          ////

          val b = analysis(program, New)
          b.analyzeWithTimeout(timeout())
          assume(b.finished)
          i.updateAnalysis(timeout())
          assume(i.finished)
          checkSubsumption(b, i)

        } catch {
          case e: VirtualMachineError =>
            System.gc()
            cancel(s"Analysis of $benchmark encountered an error: $e.")
        }
      }
    }
  }
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

  class Analysis(program: SchemeExp, version: Version) extends BaseAnalysis(program) with BigStepModFSemantics with GlobalStore[SchemeExp] {
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with BigStepModFIntra with GlobalStoreIntra {
      override protected def eval(exp: SchemeExp): EvalM[Value] = exp match {
        case SchemeCodeChange(e, _, _) if version == Old => eval(e)
        case SchemeCodeChange(_, e, _) if version == New => eval(e)
        case _                                           => super.eval(exp)
      }
    }
  }

  def analysis(e: SchemeExp, version: Version) = new Analysis(e, version)
  def incAnalysis(e: SchemeExp) = new IncrementalAnalysis(e)

  def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag] =
    if (c.rank <= 1 || c == allOptimisations) Seq(IncrementalTest) else Seq(IncrementalTest, SlowTest)

  def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size == a.visited.size, "The visited sets of both analyses are not equally big.")
    assert(i.store.size == a.store.size, "Both analyses contain a different number of elements in their store.")
    assert(i.visited.diff(a.visited).isEmpty,
           "The visited sets of both analyses differ after the initial run."
    ) // If the size is equal, this checks also the converse assertion.
    a.store.foreach { case (addr, av) =>
      val iv = i.store.getOrElse(addr, i.lattice.bottom)
      assert(av == iv, s"Store mismatch at $addr: $av <=> $iv.")
    } // No need to remove annotations (should not be in the store).
  }

  def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size >= a.visited.size, "The incremental analysis did not visit the same components than the full reanalysis.")
    assert(i.store.size >= a.store.size, "The incrementally updated store is smaller than the store after a full reanalysis.")
    assert(a.visited.diff(i.visited).isEmpty,
           "The visited set of the incremental update does not subsume the visited set of the full reanalysis."
    ) // If the size is equal, this checks also the converse assertion.
    a.store.foreach { case (addr, av) =>
      val iv = i.store.getOrElse(addr, i.lattice.bottom)
      assert(a.lattice.subsumes(iv, av), s"Store mismatch at $addr: $av is not subsumed by $iv.")
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

  class Analysis(program: SchemeExp, version: Version)
      extends BaseModConcAnalysis(program)
         with SmallStepModConcSemantics
         with GlobalStore[SchemeExp] {
    override def intraAnalysis(
        cmp: SmallStepModConcComponent
      ): KCFAIntra = new IntraAnalysis(cmp) with SmallStepIntra with KCFAIntra with GlobalStoreIntra {
      override protected def evaluate(
          exp: Exp,
          env: Env,
          stack: Stack
        ): Set[State] = exp match {
        case SchemeCodeChange(e, _, _) if version == Old => Set(Eval(e, env, stack))
        case SchemeCodeChange(_, e, _) if version == New => Set(Eval(e, env, stack))
        case _                                           => super.evaluate(exp, env, stack)
      }
    }
  }

  def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag] = Seq(IncrementalTest, SlowTest)

  def analysis(e: SchemeExp, version: Version) = new Analysis(e, version)
  def incAnalysis(e: SchemeExp) = new IncrementalAnalysis(e)

  def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size == a.visited.size, "The visited sets of both analyses are not equally big.")
    assert(i.store.size == a.store.size, "Both analyses contain a different number of elements in their store.")
    assert(i.visited.diff(a.visited).isEmpty,
           "The visited sets of both analyses differ after the initial run."
    ) // If the size is equal, this checks also the converse assertion.
    a.store.foreach { case (addr, av) =>
      val iv = i.store.getOrElse(addr, i.lattice.bottom)
      assert(av == iv, s"Store mismatch at $addr: $av <=> $iv.")
    } // No need to remove annotations (should not be in the store).
  }

  def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit = {
    assert(i.visited.size >= a.visited.size, "The incremental analysis did not visit the same components than the full reanalysis.")
    assert(i.store.size >= a.store.size, "The incrementally updated store is smaller than the store after a full reanalysis.")
    assert(a.visited.diff(i.visited).isEmpty,
           "The visited set of the incremental update does not subsume the visited set of the full reanalysis."
    ) // If the size is equal, this checks also the converse assertion.
    a.store.foreach { case (addr, av) =>
      val iv = i.store.getOrElse(addr, i.lattice.bottom)
      assert(a.lattice.subsumes(iv, av), s"Store mismatch at $addr: $av is not subsumed by $iv.")
    }
  }
}
