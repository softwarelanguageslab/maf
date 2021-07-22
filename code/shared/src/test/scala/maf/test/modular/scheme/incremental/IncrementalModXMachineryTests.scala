package maf.test.modular.scheme.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.scheme._
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental._
import maf.modular.scheme.SchemeDomain
import maf.modular._
import maf.modular.incremental.scheme.lattice._
import maf.modular.incremental.scheme.modconc._
import maf.modular.incremental.scheme.modf._
import maf.modular.scheme.modf._
import maf.modular.scheme.ssmodconc.KKallocModConc
import maf.modular.worklist._
import maf.test._
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag
import org.scalatest.propspec.AnyPropSpec

import scala.concurrent.duration._

class IncrementalModXMachineryTests extends AnyPropSpec {
  type Benchmark = String
  type Analysis = ModAnalysis[SchemeExp]
    with GlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SchemeDomain
    with IncrementalModAnalysis[SchemeExp]

  def testTags(b: Benchmark): Seq[Tag] = if (b.contains("cscheme")) Seq(IncrementalTest, SlowTest) else Seq(IncrementalTest)

  def parse(benchmark: Benchmark): SchemeExp = CSchemeParser.parse(Reader.loadFile(benchmark))

  def sequentialAnalysis(e: SchemeExp): Analysis =
    new ModAnalysis[SchemeExp](e)
      with StandardSchemeModFComponents
      with SchemeModFCallSiteSensitivity
      with SchemeModFSemantics
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain
      with IncrementalGlobalStore[SchemeExp] {
      var configuration: IncrementalConfiguration = allOptimisations
      override def intraAnalysis(
          cmp: Component
        ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
    }

  def parallelAnalysis(e: SchemeExp): Analysis =
    new ModAnalysis[SchemeExp](e)
      with KKallocModConc
      with IncrementalSchemeModConcSmallStepSemantics
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeTypeDomain
      with IncrementalGlobalStore[SchemeExp] {

      val k = 0
      var configuration: IncrementalConfiguration = allOptimisations

      override def intraAnalysis(
          cmp: Component
        ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis
    }

  def timeout(): Timeout.T = Timeout.start(Duration(5, MINUTES))

  def runAnalysis(makeAnalysis: SchemeExp => Analysis, benchmark: Benchmark): Analysis = {
    val a = makeAnalysis(parse(benchmark))
    try {
      val to = timeout()
      a.analyzeWithTimeout(to)
      assume(a.finished, "Analysis timed out.")
    } catch {
      case e: VirtualMachineError =>
        System.gc()
        cancel(s"Analysis of $benchmark encountered an error: $e.")
    }
    a
  }

  /** Tests whether the analysis correctly infers components that should be reanalysed. */
  def testUpdatedComponents(): Unit = {
    val expectedResults: List[(String, SchemeExp => Analysis, Set[String])] = List(
      ("test/changes/scheme/fib.scm", sequentialAnalysis, Set("λ@1:16 [[4:50]]", "λ@1:16 [[4:23]]", "λ@1:16 [[5:4]]")),
      ("test/changes/scheme/satRem.scm", sequentialAnalysis, Set("λ@15:23 [[11:8]]", "main", "λ@15:23 [[11:15]]")),
      ("test/changes/scheme/ring-rotate.scm", sequentialAnalysis, Set("main", "print-ring [[35:2]]")),
      ("test/changes/cscheme/threads/actors.scm", parallelAnalysis, Set("ThreadComponent((act state name), ())")),
      ("test/changes/cscheme/threads/mcarlo.scm", parallelAnalysis, Set("main"))
    )
    expectedResults.foreach { case (benchmark, makeAnalysis, affected) =>
      property(s"Incremental analysis finds correct updated expressions for $benchmark.", testTags(benchmark): _*) {
        val a = runAnalysis(makeAnalysis, benchmark)
        val directlyAffected: Set[String] = a.findUpdatedExpressions(a.program).flatMap(a.mapping).map(_.toString)
        assert(directlyAffected.size == affected.size)
        assert(directlyAffected.forall(affected.contains), s" - Directly affected component mismatch.")
      }
    }
  }

  /** Tests whether the deletion of components works correctly using an artificial example. */
  def testComponentDeletion(): Unit = {
    property("Component deletion works on an artificial example", IncrementalTest) {
      val program: String =
        """(define (a)
          |  (<change> (b) #t)
          |  (d))
          |(define (b)
          |  (c)
          |  (a)
          |  (e))
          |(define (c)
          |  (<change> (b) #t)
          |  (d)
          |  (<change> (e) #t))
          |(define (d)
          |  (<change> (e) #t))
          |(define (e)
          |  (<change> #t (d))
          |  (b))
          |(a)""".stripMargin
      val a = new ModAnalysis[SchemeExp](CSchemeParser.parse(program))
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity // Different
        with SchemeModFSemantics
        with LIFOWorklistAlgorithm[SchemeExp]
        with IncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeConstantPropagationDomain // Different
        with IncrementalGlobalStore[SchemeExp] {
        var configuration: IncrementalConfiguration = ci_di_wi
        override def intraAnalysis(
            cmp: Component
          ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
      }
      try {
        a.analyzeWithTimeout(timeout())
        assume(a.finished)
        assert(a.visited.size == 4) // main, a, b, c. Not d, e because the analysis should stop at bottom.
        assert(a.visited.map(_.toString).diff(Set("main", "a [ε]", "b [ε]", "c [ε]")).isEmpty)
        a.updateAnalysis(timeout())
        a.deleteDisconnectedComponents()
        assert(a.visited.size == 3) // main, a, d.
        assert(a.visited.map(_.toString).diff(Set("main", "a [ε]", "d [ε]")).isEmpty)
      } catch {
        case e: VirtualMachineError =>
          System.gc()
          cancel(s"Analysis of program encountered an error: $e.")
      }
    }
  }

  testComponentDeletion()
  testUpdatedComponents()
}
