package maf.modular.incremental.scheme

import maf.language.scheme._
import maf.modular._
import maf.modular.incremental._
import maf.modular.incremental.scheme.modconc._
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.scheme.ssmodconc._
import maf.modular.worklist.LIFOWorklistAlgorithm

/**
 * Provides instantiations of incremental analyses.
 * @note By having instantiations listed here, it is ensured that no changes break the instantiation of incremental analyses.
 */
object SchemeAnalyses {

  /* ******************* */
  /* ***** ModConc ***** */
  /* ******************* */

  abstract class BaseModConcAnalysis(prg: SchemeExp)
      extends ModAnalysis[SchemeExp](prg)
         with KKallocModConc
         with IncrementalSchemeModConcSmallStepSemantics
         with LIFOWorklistAlgorithm[SchemeExp]
         with IncrementalGlobalStore[SchemeExp]

  /**
   * Builds an incremental ModConc Analysis for the given Scheme program with the following properties:
   * <ul>
   * <li>Allocated continuation addresses contain a single expression, and the previous continuation address. ({@link KKallocModConc}, k = 1).</li>
   * <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   * <li>Uses an abstract type domain ({@link SchemeTypeDomain}).</li>
   * <li>Uses small-step semantics ({@link IncrementalSchemeModConcSmallStepSemantics}).</li>
   * </ul>
   *
   * @param prg The program to construct the analysis for.
   */
  class IncrementalModConcAnalysisTypeLattice(prg: SchemeExp, val k: Int = 1) extends BaseModConcAnalysis(prg) with SchemeTypeDomain {

    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis
  }

  /**
   * Builds an incremental ModConc Analysis for the given Scheme program with the following properties:
   * <ul>
   * <li>Allocated continuation addresses contain a single expression, and the previous continuation address. ({@link KKallocModConc}, k = 1).</li>
   * <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   * <li>Uses an abstract type domain ({@link SchemeConstantPropagationDomain}).</li>
   * <li>Uses small-step semantics ({@link IncrementalSchemeModConcSmallStepSemantics}).</li>
   * </ul>
   *
   * @param prg The program to construct the analysis for.
   */
  class IncrementalModConcAnalysisCPLattice(prg: SchemeExp, val k: Int = 1) extends BaseModConcAnalysis(prg) with SchemeConstantPropagationDomain {

    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis
  }

  /* **************** */
  /* ***** ModF ***** */
  /* **************** */

  abstract class BaseModFAnalysis(prg: SchemeExp)
      extends ModAnalysis[SchemeExp](prg)
         with StandardSchemeModFComponents
         with SchemeModFNoSensitivity
         with SchemeModFSemantics
         with LIFOWorklistAlgorithm[SchemeExp]
         with IncrementalSchemeModFBigStepSemantics
         with IncrementalGlobalStore[SchemeExp]

  /**
   * Builds an incremental ModF Analysis for the given Scheme program with the following properties:
   * <ul>
   * <li>Uses standard scheme ModF components ({@link StandardSchemeModFComponents}).</li>
   * <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   * <li>Uses an abstract type domain ({@link SchemeTypeDomain}).</li>
   * <li>Uses big-step semantics ({@link IncrementalSchemeModFBigStepSemantics}).</li>
   * </ul>
   *
   * @param prg The program to construct the analysis for.
   */
  class IncrementalSchemeModFAnalysisTypeLattice(prg: SchemeExp) extends BaseModFAnalysis(prg) with SchemeTypeDomain {
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
  }

  // Same as the one above, but with assertions.
  class IncrementalSchemeModFAssertionAnalysisTypeLattice(prg: SchemeExp)
      extends IncrementalSchemeModFAnalysisTypeLattice(prg)
         with SchemeAssertSemantics {
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with AssertionModFIntra
  }

  /**
   * Builds an incremental ModF Analysis for the given Scheme program with the following properties:
   * <ul>
   * <li>Uses standard scheme ModF components ({@link StandardSchemeModFComponents}).</li>
   * <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   * <li>Uses an abstract type domain ({@link SchemeConstantPropagationDomain}).</li>
   * <li>Uses big-step semantics ({@link IncrementalSchemeModFBigStepSemantics}).</li>
   * </ul>
   *
   * @param prg The program to construct the analysis for.
   */
  class IncrementalSchemeModFAnalysisCPLattice(prg: SchemeExp) extends BaseModFAnalysis(prg) with SchemeConstantPropagationDomain {
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
  }

  // Same as the one above, but with assertions.
  class IncrementalSchemeModFAssertionAnalysisCPLattice(prg: SchemeExp)
      extends IncrementalSchemeModFAnalysisCPLattice(prg)
         with SchemeAssertSemantics {
    override def intraAnalysis(
        cmp: Component
      ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with AssertionModFIntra
  }

}
