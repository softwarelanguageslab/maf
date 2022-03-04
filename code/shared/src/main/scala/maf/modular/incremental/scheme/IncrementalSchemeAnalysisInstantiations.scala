package maf.modular.incremental.scheme

import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modconc.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf.*
import maf.modular.scheme.ssmodconc.*
import maf.modular.worklist.*

/**
 * Provides instantiations of incremental analyses.
 * @note
 *   By having instantiations listed here, it is ensured that no changes break the instantiation of incremental analyses.
 * @note
 *   Due to linearisation reasons, IncrementalGlobalStore must be mixed-in last.
 */
// TODO Fix linearisation "issue - IGS last" if possible?
object IncrementalSchemeAnalysisInstantiations:

    /* ******************* */
    /* ***** ModConc ***** */
    /* ******************* */

    abstract class BaseModConcAnalysisIncremental(prg: SchemeExp)
        extends ModAnalysis[SchemeExp](prg)
        with KKallocModConc
        with IncrementalSchemeModConcSmallStepSemantics
        with LIFOWorklistAlgorithm[SchemeExp]

    /**
     * Builds an incremental ModConc Analysis for the given Scheme program with the following properties: <ul> <li>Allocated continuation addresses
     * contain a single expression, and the previous continuation address. ({@link KKallocModConc}, k = 1).</li> <li>Uses a dept-first (LIFO)
     * exploration order ({@link LIFOWorklistAlgorithm}).</li> <li>Uses an abstract type domain ({@link IncrementalSchemeTypeDomain}).</li> <li>Uses
     * small-step semantics ({@link IncrementalSchemeModConcSmallStepSemantics}).</li> </ul>
     *
     * @param prg
     *   The program to construct the analysis for.
     */
    class IncrementalModConcAnalysisTypeLattice(
        prg: SchemeExp,
        var configuration: IncrementalConfiguration,
        val k: Int = 1)
        extends BaseModConcAnalysisIncremental(prg)
        with IncrementalSchemeTypeDomain
        with IncrementalGlobalStore[SchemeExp]:

        override def intraAnalysis(cmp: Component) =
          new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis

    /**
     * Builds an incremental ModConc Analysis for the given Scheme program with the following properties: <ul> <li>Allocated continuation addresses
     * contain a single expression, and the previous continuation address. ({@link KKallocModConc}, k = 1).</li> <li>Uses a dept-first (LIFO)
     * exploration order ({@link LIFOWorklistAlgorithm}).</li> <li>Uses an abstract type domain ({@link
     * IncrementalSchemeConstantPropagationDomain}).</li> <li>Uses small-step semantics ({@link IncrementalSchemeModConcSmallStepSemantics}).</li>
     * </ul>
     *
     * @param prg
     *   The program to construct the analysis for.
     */
    class IncrementalModConcAnalysisCPLattice(prg: SchemeExp, var configuration: IncrementalConfiguration, val k: Int = 1)
        extends BaseModConcAnalysisIncremental(prg)
        with IncrementalSchemeConstantPropagationDomain
        with IncrementalGlobalStore[SchemeExp]:

        override def intraAnalysis(cmp: Component) =
          new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis

    /* **************** */
    /* ***** ModF ***** */
    /* **************** */

    abstract class BaseModFAnalysisIncremental(prg: SchemeExp)
        extends ModAnalysis[SchemeExp](prg)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsM
        with LIFOWorklistAlgorithm[SchemeExp]
        with IncrementalSchemeModFBigStepSemantics

    /**
     * Builds an incremental ModF Analysis for the given Scheme program with the following properties: <ul> <li>Uses standard scheme ModF components
     * ({@link StandardSchemeModFComponents}).</li> <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li> <li>Uses an
     * abstract type domain ({@link IncrementalSchemeTypeDomain}).</li> <li>Uses big-step semantics ({@link
     * IncrementalSchemeModFBigStepSemantics}).</li> </ul>
     *
     * @param prg
     *   The program to construct the analysis for.
     */
    class IncrementalSchemeModFAnalysisTypeLattice(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends BaseModFAnalysisIncremental(prg)
        with IncrementalSchemeTypeDomain
        with IncrementalGlobalStore[SchemeExp]:
        override def intraAnalysis(cmp: Component) =
          new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

    // Same as the one above, but with assertions.
    class IncrementalSchemeModFAssertionAnalysisTypeLattice(prg: SchemeExp, configuration: IncrementalConfiguration)
        extends IncrementalSchemeModFAnalysisTypeLattice(prg, configuration)
        with SchemeAssertSemantics:
        override def intraAnalysis(cmp: Component) =
          new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with AssertionModFIntra

    /**
     * Builds an incremental ModF Analysis for the given Scheme program with the following properties: <ul> <li>Uses standard scheme ModF components
     * ({@link StandardSchemeModFComponents}).</li> <li>Uses a dept-first (LIFO) exploration order ({@link LIFOWorklistAlgorithm}).</li> <li>Uses an
     * abstract type domain ({@link IncrementalSchemeConstantPropagationDomain}).</li> <li>Uses big-step semantics ({@link
     * IncrementalSchemeModFBigStepSemantics}).</li> </ul>
     *
     * @param prg
     *   The program to construct the analysis for.
     */
    class IncrementalSchemeModFAnalysisCPLattice(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends BaseModFAnalysisIncremental(prg)
        with IncrementalSchemeConstantPropagationDomain
        with IncrementalGlobalStore[SchemeExp]:
        override def intraAnalysis(cmp: Component) =
          new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

    // Same as the one above, but with assertions.
    class IncrementalSchemeModFAssertionAnalysisCPLattice(prg: SchemeExp, configuration: IncrementalConfiguration)
        extends IncrementalSchemeModFAnalysisCPLattice(prg, configuration)
        with SchemeAssertSemantics:
        override def intraAnalysis(cmp: Component) =
          new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with AssertionModFIntra
