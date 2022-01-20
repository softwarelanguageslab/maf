package maf.cli.experiments

import maf.language.scheme._
import maf.modular._
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

    def modflocalAnalysis(prg: SchemeExp, k: Int) =
      new SchemeModFLocal(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalCallSiteSensitivity(k)
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalAnalysisResults
    def modflocalFSAnalysis(prg: SchemeExp, k: Int) =
      new SchemeModFLocalFS(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalCallSiteSensitivity(k)
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalFSAnalysisResults
    def modflocalAnalysisAdaptiveA(prg: SchemeExp, k: Int, n: Int) =
      new SchemeModFLocal(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalCallSiteSensitivity(k)
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalAnalysisResults
        with SchemeModFLocalAdaptiveWideningPolicyA(n)
    def modFlocalAnalysisSelective(prg: SchemeExp, k: Int, widened: Set[Address]) =
      new SchemeModFLocal(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalCallSiteSensitivity(k)
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalAnalysisResults:
          override def customPolicy(adr: Adr): AddrPolicy =
            if widened(adr) then AddrPolicy.Widened else AddrPolicy.Local
    def modFlocalAnalysisWidened(prg: SchemeExp, k: Int) =
      new SchemeModFLocal(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalCallSiteSensitivity(k)
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalAnalysisResults:
          override def customPolicy(adr: Adr): AddrPolicy = AddrPolicy.Widened

    def scvModAnalysis(prg: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
          with ScvBigStepSemantics
          with SchemeConstantPropagationDomain
          with StandardSchemeModFComponents
          with LIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFSemanticsM
          with ScvOneContextSensitivity:
            override def intraAnalysis(cmp: Component) = new IntraScvSemantics(cmp)
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver

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
          with ScvBigStepWithProvides
          with ScvWithStructs
          with SchemeConstantPropagationDomain
          with StandardSchemeModFComponents
          with LIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFSemanticsM
          with ScvOneContextSensitivity:
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp) with IntraScvSemanticsWithProvides with IntraScvSemanticsWithStructs
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver
