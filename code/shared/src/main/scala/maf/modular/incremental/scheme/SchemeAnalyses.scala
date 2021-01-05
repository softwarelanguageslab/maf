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

  /**
   * Builds an incremental ModConc Analysis for the given Scheme program with the following properties:
   * <ul>
   *   <li>Allocated continuation addresses contain a single expression, and the previous continuation address. ({@link KKallocModConc}, k = 1).</li>
   *   <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   *   <li>Uses an abstract type domain ({@link SchemeTypeDomain}).</li>
   *   <li>Uses small-step semantics ({@link IncrementalSchemeModConcSmallStepSemantics}).</li>
   * </ul>
   * @param prg The program to construct the analysis for.
   */
  class IncrementalModConcAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                      with KKallocModConc
                                                      with IncrementalSchemeModConcSmallStepSemantics
                                                      with LIFOWorklistAlgorithm[SchemeExp]
                                                      with SchemeTypeDomain {
    val k = 1
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra
  }

  /**
   * Builds an incremental ModConc Analysis for the given Scheme program with the following properties:
   * <ul>
   *   <li>Allocated continuation addresses contain a single expression, and the previous continuation address. ({@link KKallocModConc}, k = 1).</li>
   *   <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   *   <li>Uses an abstract type domain ({@link SchemeConstantPropagationDomain}).</li>
   *   <li>Uses small-step semantics ({@link IncrementalSchemeModConcSmallStepSemantics}).</li>
   * </ul>
   * @param prg The program to construct the analysis for.
   */
  class IncrementalModConcCPAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                        with KKallocModConc
                                                        with IncrementalSchemeModConcSmallStepSemantics
                                                        with LIFOWorklistAlgorithm[SchemeExp]
                                                        with SchemeConstantPropagationDomain {
    val k = 1
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra
  }

  /**
   * Builds an incremental ModF Analysis for the given Scheme program with the following properties:
   * <ul>
   *   <li>Uses standard scheme ModF components ({@link StandardSchemeModFComponents}).</li>
   *   <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   *   <li>Uses an abstract type domain ({@link SchemeTypeDomain}).</li>
   *   <li>Uses big-step semantics ({@link IncrementalSchemeModFBigStepSemantics}).</li>
   * </ul>
   * @param prg The program to construct the analysis for.
   */
  class IncrementalSchemeModFAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                         with StandardSchemeModFComponents
                                                         with SchemeModFNoSensitivity
                                                         with SchemeModFSemantics
                                                         with LIFOWorklistAlgorithm[SchemeExp]
                                                         with SchemeTypeDomain
                                                         with IncrementalSchemeModFBigStepSemantics {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra
  }

  /**
   * Builds an incremental ModF Analysis for the given Scheme program with the following properties:
   * <ul>
   *   <li>Uses standard scheme ModF components ({@link StandardSchemeModFComponents}).</li>
   *   <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li>
   *   <li>Uses an abstract type domain ({@link SchemeConstantPropagationDomain}).</li>
   *   <li>Uses big-step semantics ({@link IncrementalSchemeModFBigStepSemantics}).</li>
   * </ul>
   * @param prg The program to construct the analysis for.
   */
  class IncrementalSchemeModFCPAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                           with StandardSchemeModFComponents
                                                           with SchemeModFNoSensitivity
                                                           with SchemeModFSemantics
                                                           with LIFOWorklistAlgorithm[SchemeExp]
                                                           with SchemeConstantPropagationDomain
                                                           with IncrementalSchemeModFBigStepSemantics {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra
  }

  // With store invalidation.
  class IncrementalModConcCPAnalysisStoreOpt(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                                with KKallocModConc
                                                                with IncrementalSchemeModConcSmallStepSemantics
                                                                with LIFOWorklistAlgorithm[SchemeExp]
                                                                with SchemeConstantPropagationDomain
                                                                with IncrementalGlobalStore[SchemeExp] {
                                                                val k = 1 // TODO Perhaps make a parameter of the class.
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis
  }

  class IncrementalSchemeModFCPAnalysisStoreOpt(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                                   with StandardSchemeModFComponents
                                                                   with SchemeModFNoSensitivity
                                                                   with SchemeModFSemantics
                                                                   with LIFOWorklistAlgorithm[SchemeExp]
                                                                   with SchemeConstantPropagationDomain
                                                                   with IncrementalSchemeModFBigStepSemantics
                                                                   with IncrementalReturnValue[SchemeExp] {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalReturnValueIntraAnalysis
  }
}
