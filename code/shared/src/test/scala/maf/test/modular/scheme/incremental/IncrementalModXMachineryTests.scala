package maf.test.modular.scheme.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental._
import maf.modular.scheme.SchemeDomain
import maf.modular._
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf._
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test.IncrementalTest
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

  def testTags(): Seq[Tag] = Seq(IncrementalTest)

  def analysis(benchmark: Benchmark): Analysis =
    new ModAnalysis[SchemeExp](CSchemeParser.parse(Reader.loadFile(benchmark)))
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
  def timeout(): Timeout.T = Timeout.start(Duration(3, MINUTES))

  /** Tests whether the analysis correctly infers components that should be reanalysed. */
  def testUpdatedComponents(): Unit = {
    val expectedResults: Map[String, Set[String]] = Map(
      ("test/changes/scheme/fib.scm", Set("λ@1:16 [[4:50]]", "λ@1:16 [[4:23]]", "λ@1:16 [[5:4]]")),
      ("test/changes/scheme/satRem.scm", Set("λ@15:23 [[11:8]]", "main", "λ@15:23 [[11:15]]")),
      ("test/changes/scheme/ring-rotate.scm", Set("main", "print-ring [[35:2]]"))
    )
    expectedResults.foreach { case (benchmark, affected) =>
      property(s"Incremental analysis finds correct updated expressions for $benchmark.", testTags(): _*) {
        val a = analysis(benchmark)
        try {
          val to = timeout()
          a.analyzeWithTimeout(to)
          assume(a.finished, "Analysis timed out.")
          val directlyAffected: Set[String] = a.findUpdatedExpressions(a.program).flatMap(a.mapping).map(_.toString)
          assert(directlyAffected.size == affected.size)
          assert(directlyAffected.forall(affected.contains), s" - Directly affected component mismatch.")
        } catch {
          case e: VirtualMachineError =>
            System.gc()
            cancel(s"Analysis of $benchmark encountered an error: $e.")
        }
      }
    }
  }

  testUpdatedComponents()
}
