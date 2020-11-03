package maf.modular.incremental.scheme

import maf.language.scheme.SchemeExp
import maf.modular._
import maf.modular.incremental.scheme.modconc._
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.SchemeTypeDomain
import maf.modular.scheme.modf._
import maf.modular.scheme.ssmodconc._
import maf.modular.worklist.LIFOWorklistAlgorithm

/**
 * Provides instantiations of incremental analyses.
 * @note By having instantiations listed here, it is ensured that no changes break the instantiation of incremental analyses.
 */
object AnalysisBuilder {

  /**
   * Builds an incremental ModConc Analysis for Scheme with the following properties:
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
   * Builds an incremental ModF Analysis for Scheme with the following properties:
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
}
