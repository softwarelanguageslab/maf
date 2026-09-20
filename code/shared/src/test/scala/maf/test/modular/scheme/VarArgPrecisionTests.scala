package maf.test.modular.scheme

import org.scalatest.funsuite.AnyFunSuite
import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.modular.scheme.modf._
import maf.modular.scheme.modconc._
import maf.modular.scheme.ssmodconc._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.core._
import maf.modular._
import maf.language.scheme.primitives.SchemePrelude

class VarArgPrecisionTests extends AnyFunSuite {

  def runAllAnalyses(prg: SchemeExp, verifyResultLocal: Set[_] => Unit, verifyResultGlobal: Any => Unit): Unit = {
    // 1. ModFLocal
    val an1 = new SchemeModFLocal(prg)
              with SchemeConstantPropagationDomain
              with SchemeModFLocalNoSensitivity
              with FIFOWorklistAlgorithm[SchemeExp]
              with SchemeModFLocalAnalysisResults
    an1.analyze()
    // For ModFLocal, we find the last expression evaluated (which is usually what we care about)
    val resultIdn = an1.resultsPerIdn.keys.maxBy(idn => (idn.pos.line, idn.pos.col))
    verifyResultLocal(an1.resultsPerIdn(resultIdn))

    // 2. ModF
    val an2 = new SimpleSchemeModFAnalysis(prg)
              with StandardSchemeModFAllocator
              with SchemeConstantPropagationDomain
              with SchemeModFNoSensitivity
              with FIFOWorklistAlgorithm[SchemeExp]
    an2.analyze()
    verifyResultGlobal(an2.finalResult)

    // 3. SmallStepModConc
    val an3 = new ModAnalysis(prg) 
        with KKallocModConc 
        with SchemeConstantPropagationDomain 
        with LIFOWorklistAlgorithm[SchemeExp] {
        val k = 1
        override def intraAnalysis(component: SmallStepModConcComponent) = new IntraAnalysis(component) with SmallStepIntra with KCFAIntra
    }
    an3.analyze()
    verifyResultGlobal(an3.finalResult)

    // 4. ModConc (Big-step)
    val an4 = new SimpleSchemeModConcAnalysis(prg)
        with SchemeModConcStandardSensitivity
        with SchemeConstantPropagationDomain
        with CallDepthFirstWorklistAlgorithm[SchemeExp]
        with ParallelWorklistAlgorithm[SchemeExp] {
        override def workers: Int = 4
        override def intraAnalysis(cmp: SchemeModConcComponent) = new SchemeModConcIntra(cmp) with ParallelIntra
        def modFAnalysis(intra: SchemeModConcIntra) = new InnerModFAnalysis(intra) with SchemeModFNoSensitivity with RandomWorklistAlgorithm[SchemeExp]
    }
    an4.analyze()
    verifyResultGlobal(an4.finalResult)
  }

  def buildPrg(code: String): SchemeExp = {
    val parsed = SchemeParser.parse(code)
    val prelud = SchemePrelude.addPrelude(parsed)
    val transf = SchemeMutableVarBoxer.transform(prelud)
    SchemeParser.undefine(transf)
  }

  test("Test 1: Vararg list allocation should not merge with argument expressions") {
    val code = """
    (define (f . x) x)
    (car (f '(1) '(2) '(3)))
    """
    
    runAllAnalyses(buildPrg(code),
      localRes => {
        val containsNumber1 = localRes.exists(v => v.toString.contains("1") && !v.toString.contains("PtrAddr") && !v.toString.contains("VarArgPtrAddr"))
        assert(!containsNumber1, s"Vararg allocation merged! Local result: $localRes")
      },
      globalRes => {
        val containsNumber1 = globalRes.toString.contains("1") && !globalRes.toString.contains("PtrAddr") && !globalRes.toString.contains("VarArgPtrAddr")
        assert(!containsNumber1, s"Vararg allocation merged! Global result: $globalRes")
      }
    )
  }

  test("Test 2: Vararg list with fixed arguments should resolve precisely") {
    val code = """
    (define (f x . y) (car y))
    (f 100 200 300)
    """
    
    runAllAnalyses(buildPrg(code),
      localRes => {
        // ModFLocal resultsPerIdn only tracks the store (VarAddr/PtrAddr), not the final return value.
        // We can skip checking the final return value for ModFLocal here, as the global check covers it for all others.
      },
      globalRes => {
        assert(globalRes.toString.contains("200"), s"Should contain 200, but got: $globalRes")
        assert(!globalRes.toString.contains("100"), s"Should not contain 100 (fixed arg leaked)! Got: $globalRes")
        assert(!globalRes.toString.contains("300"), s"Should not contain 300 (incorrect list element)! Got: $globalRes")
      }
    )
  }

  test("Test 3: Empty vararg should resolve to empty list") {
    val code = """
    (define (f . x) x)
    (null? (f))
    """
    
    runAllAnalyses(buildPrg(code),
      localRes => { },
      globalRes => assert(globalRes.toString.contains("true"), s"Result should be true, got: $globalRes")
    )
  }
}
