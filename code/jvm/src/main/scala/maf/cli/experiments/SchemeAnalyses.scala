package maf.cli.experiments

import maf.language.scheme._
import maf.modular.scheme.aam._
import maf.modular._
import maf.language.symbolic.lattices.*
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.util.MonoidImplicits.setMonoid
import maf.modular.scheme.modflocal._
import maf.modular.scheme.modconc._
import maf.modular.worklist._
import maf.modular.scv._
import maf.cli.modular.scv.JVMSatSolver
import maf.language.scheme.lattices.SchemeLattice
import maf.core.Address
import scala.reflect.ClassTag
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.modular.scheme.modactor.mirrors.ModActorWithMirrors
import maf.modular.scheme.modactor.mirrors.SimpleModActorWithMirrors
import maf.language.racket.RacketLoaderSemantics
import maf.language.racket.RacketLoader

object SchemeAnalysesBoundedDomain:
    object NoSensitivity:
        def boundAnalysis(
            bnd: Int
          )(
            prg: SchemeExp
          ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeBoundedDomain(bnd) with LIFOWorklistAlgorithm[SchemeExp] {
            override def toString = "no-sensitivity"
        }
    object CallSiteSensitivity:
        def boundAnalysis(
            bnd: Int
          )(
            prg: SchemeExp
          ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFCallSiteSensitivity with SchemeBoundedDomain(bnd) with LIFOWorklistAlgorithm[SchemeExp] {
            val bound = bnd
            override def toString = "call-site-sensitivity"
        }
    object TwoCallSiteSensitivity:
        def boundAnalysis(
            bnd: Int
          )(
            prg: SchemeExp
          ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFCallSiteSensitivity with SchemeBoundedDomain(bnd) with LIFOWorklistAlgorithm[SchemeExp] {
            override def toString = "call-site-sensitivity"
        }

object SchemeAnalyses:

    // Incremental analyses in maf.modular.incremental.scheme.SchemeAnalyses

    def contextInsensitiveAnalysis(
        prg: SchemeExp
      ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeConstantPropagationDomain with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "no-sensitivity"
    }

    //def contextInsensitiveAnalysisRacket(
    //    prg: SchemeExp
    //  ) = new SimpleSchemeModFAnalysis(prg)
    //    with SchemeModFNoSensitivity
    //    with SchemeConstantPropagationDomain
    //    with FIFOWorklistAlgorithm[SchemeExp]
    //    with RacketLoaderSemantics {
    //    override def toString = "no-sensitivity"
    //}
    def callSiteContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "call-site-sensitivity"
    }
    def kCFAAnalysis(prg: SchemeExp, kcfa: Int) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = s"kCFA (k = $kcfa)"
        val k = kcfa
    }
    def fullArgContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFFullArgumentSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "full-argument-sensitivity"
    }
    def parallelKCFAAnalysis(
        prg: SchemeExp,
        n: Int,
        kcfa: Int
      ) = new ModAnalysis(prg)
        with SchemeModFSemanticsM
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with CallDepthFirstWorklistAlgorithm[SchemeExp]
        with ParallelWorklistAlgorithm[SchemeExp]
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {

        override def toString = s"parallel k-CFA (n = $n ; k = $kcfa)"
        val k = kcfa
        override def workers = n
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
    }
    def modConcAnalysis(prg: SchemeExp, kcfa: Int) = new SimpleSchemeModConcAnalysis(prg)
        with SchemeModConcStandardSensitivity
        with SchemeConstantPropagationDomain
        with CallDepthFirstWorklistAlgorithm[SchemeExp] {
        override def toString = s"base modconc"
        override def modFAnalysis(
            intra: SchemeModConcIntra
          ) = new InnerModFAnalysis(intra) with SchemeModFKCallSiteSensitivity with CallDepthFirstWorklistAlgorithm[SchemeExp] {
            val k = kcfa
        }
    }
    def parallelModConc(
        prg: SchemeExp,
        n: Int,
        m: Int,
        kcfa: Int
      ) = new SimpleSchemeModConcAnalysis(prg)
        with SchemeModConcStandardSensitivity
        with SchemeConstantPropagationDomain
        with CallDepthFirstWorklistAlgorithm[SchemeExp]
        with ParallelWorklistAlgorithm[SchemeExp] {
        override def workers = n
        override def toString = s"parallel modconc (n = $n ; m = $m)"
        override def intraAnalysis(cmp: Component) = new SchemeModConcIntra(cmp) with ParallelIntra
        override def modFAnalysis(
            intra: SchemeModConcIntra
          ) = new InnerModFAnalysis(intra)
            with SchemeModFKCallSiteSensitivity
            with CallDepthFirstWorklistAlgorithm[SchemeExp]
            with ParallelWorklistAlgorithm[SchemeExp] {
            val k = kcfa
            override def workers = m
            override def intraAnalysis(cmp: SchemeModFComponent) = new InnerModFIntra(cmp) with ParallelIntra
        }
    }

    def aamGCAnalysis(prg: SchemeExp, k: Int) = 
      new SchemeAAMGCAnalysis(prg, k)

    def modflocalAnalysis(prg: SchemeExp, k: Int) =
        new SchemeModFLocal(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(k)
            with FIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFLocalAnalysisResults

    def modflocalFSAnalysis(prg: SchemeExp, k: Int, gc: Boolean = true) =
      new SchemeModFLocalFS(prg, gc)
          with SchemeConstantPropagationDomain
          with SchemeModFLocalCallSiteSensitivity(k)
          with FIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFLocalFSAnalysisResults

    def modfADIAnalysis(prg: SchemeExp, k: Int) = 
      new SchemeModFADIAnalysis(prg, k)
          with SchemeModFADIAnalysisResults

    // Flow sensitive analysis
    def modFFlowSensitive(prg: SchemeExp) =
        new SimpleFlowSensitiveAnalysis(prg)

    /**
     * SCV analysis with Racket features:
     *
     *   - provide/contract
     *   - structs
     */
    def scvModAnalysisWithRacketFeatures(prg: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
            with ScvBigStepSemantics
            with SymbolicSchemeConstantPropagationDomain
            with StandardSchemeModFComponents
            with LIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFSemanticsM
            with ScvOneContextSensitivity(0)
            with ScvBigStepWithProvides
            with ScvWithStructs
            with ScvIgnoreFreshBlame:
            //with UnstableWideningWithMinimum(2):
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp) with IntraScvSemanticsWithProvides with IntraScvSemanticsWithStructs with IntraScvIgnoreFreshBlames
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver(this)

    def scvModAnalysisFunctionSummaryTopSort(prg: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
            with ScvBigStepSemantics
            with SymbolicSchemeConstantPropagationDomain
            with StandardSchemeModFComponents
            with LIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFSemanticsM
            with ScvOneContextSensitivity(0)
            with ScvBigStepWithProvides
            with ScvWithStructs
            with ScvIgnoreFreshBlame
            with FunctionSummaryAnalysis
            with TopSortPropagationPhase // ADDED
            with NoCompositionIfCycle
            with maf.modular.scv.CompositionForContracts
            with FunctionSummaryAnalysisWithMainBoundary
            with ScvArgumentSensitivity:
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp)
                with IntraScvSemanticsWithProvides
                with IntraScvSemanticsWithStructs
                with IntraScvIgnoreFreshBlames
                with NoCompositionIfCycleIntra
                with FunctionSummaryIntra
                with CompositionForContractsIntra
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver(this)

    def scvModAnalysisFunctionSummary(prg: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
            with ScvBigStepSemantics
            with SymbolicSchemeConstantPropagationDomain
            with StandardSchemeModFComponents
            with LIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFSemanticsM
            with ScvOneContextSensitivity(0)
            with ScvBigStepWithProvides
            with ScvWithStructs
            with ScvIgnoreFreshBlame
            with FunctionSummaryAnalysis
            with NoCompositionIfCycle
            with maf.modular.scv.CompositionForContracts
            with FunctionSummaryAnalysisWithMainBoundary
            with ScvArgumentSensitivity:
            //with UnstableWideningWithMinimum(2):
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp)
                with IntraScvSemanticsWithProvides
                with IntraScvSemanticsWithStructs
                with IntraScvIgnoreFreshBlames
                with NoCompositionIfCycleIntra
                with FunctionSummaryIntra
                with CompositionForContractsIntra
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver(this)

    def modActorAnalysis(prg: SchemeExp) =
        new SimpleSchemeModActorAnalysis(prg)

    def modActorWithMirrors(prg: SchemeExp) =
        new SimpleModActorWithMirrors(prg)

    //def scvModAnalysisWithRacketFeaturesWithPathSensitiveStore(prg: SchemeExp) =
    //    import maf.modular.scv.ScvSymbolicStore.given
    //    new ModAnalysis(prg)
    //        with ScvBigStepSemantics
    //        with SymbolicSchemeConstantPropagationDomain
    //        with StandardSchemeModFComponents
    //        with LIFOWorklistAlgorithm[SchemeExp]
    //        with SchemeModFSemanticsM
    //        with ScvOneContextSensitivity(0)
    //        with ScvBigStepWithProvides
    //        with ScvWithStructs
    //        with ScvFullPathSensitivity
    //        with ScvIgnoreFreshBlame
    //        with UnstableWideningWithMinimum(2):
    //        protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

    //        override def intraAnalysis(
    //            cmp: Component
    //          ) = new IntraScvSemantics(cmp)
    //            with IntraScvSemanticsWithProvides
    //            with IntraScvSemanticsWithStructs
    //            with ScvFullPathSensitivityIntra
    //            with IntraScvIgnoreFreshBlames
    //            with IntraWidening

    //        override val sat: ScvSatSolver[Value] =
    //            given SchemeLattice[Value, Address] = lattice
    //            new JVMSatSolver(this)

    /**
     * An analysis with full path sensitivity, with widen of the path condition and symbolic store, but by replacing the path condition and symbolic
     * store with a lexical version
     */
    def scvModAnalysisRktFsR(prg: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
            with ScvBigStepSemantics
            with SymbolicSchemeConstantPropagationDomain
            with StandardSchemeModFComponents
            with LIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFSemanticsM
            with ScvOneContextSensitivity(0)
            with ScvBigStepWithProvides
            with ScvWithStructs
            with ScvFullPathSensitivity
            with ScvIgnoreFreshBlame
            with UnstableWideningWithMinimum(2)
            with RemovePathCondition:
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp)
                with IntraScvSemanticsWithProvides
                with IntraScvSemanticsWithStructs
                with ScvFullPathSensitivityIntra
                with IntraScvIgnoreFreshBlames
                with IntraWidening

            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver(this)

//def scvModAnalysisWithRacketFeaturesWithIncomingFlow(prg: SchemeExp) =
//    import maf.modular.scv.ScvSymbolicStore.given
//    new ModAnalysis(prg)
//        with ScvBigStepSemantics
//        with SymbolicSchemeConstantPropagationDomain
//        with StandardSchemeModFComponents
//        with LIFOWorklistAlgorithm[SchemeExp]
//        with SchemeModFSemanticsM
//        with ScvOneContextSensitivity(0)
//        with ScvBigStepWithProvides
//        with ScvWithStructs
//        with ScvIgnoreFreshBlame
//        with UnstableWideningWithMinimum(2):
//        protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

//        override def intraAnalysis(
//            cmp: Component
//          ) = new IntraScvSemantics(cmp)
//            with IntraScvSemanticsWithProvides
//            with IntraScvSemanticsWithStructs
//            with IntraScvIgnoreFreshBlames
//            with IntraWidening

//        override val sat: ScvSatSolver[Value] =
//            given SchemeLattice[Value, Address] = lattice
//            new JVMSatSolver(this)
