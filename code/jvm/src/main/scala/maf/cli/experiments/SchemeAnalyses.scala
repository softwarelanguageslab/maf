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
import maf.modular.worklist._
import maf.language.scheme.lattices.SchemeLattice
import maf.core.Address
import scala.reflect.ClassTag
import maf.language.racket.RacketLoaderSemantics
import maf.language.racket.RacketLoader
import maf.cli.experiments.clients._

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

    def contextInsensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg) 
        with SchemeModFNoSensitivity 
        with SchemeConstantPropagationDomain 
        with FIFOWorklistAlgorithm[SchemeExp] {
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

    // selective context-sensitive analysis
    def selectiveKCFAAnalysis(prg: SchemeExp, kcfas: Map[String, Int]) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFSelectiveKCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "selective kCFA"
        val ks = kcfas
    }

    // adaptive context-sensitive analysis
    def selectMostContextsAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectMostContexts
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity"
    }

    def selectBudgetAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectBudget
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity"
    }
    def randomAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyRandom
      with SelectMostContexts
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (when: random)"
    }

    def alwaysAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyAlways
      with SelectMostContexts
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (when: always)"
    }

    // variations on what to adapt
    def selectRandomAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectRandom
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (select: random)"
    }

    def selectMostDependenciesAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectMostDependencies
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (select: most dependencies)"
    }

    def selectLeastDependenciesAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectLeastDependencies
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (select: least dependencies)"
    }

    def selectImpreciseAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectImprecise
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (select: imprecise)"
    }

    def selectDifferentValuesAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with TooManyCost
      with SelectDifferentValues
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (select: different values)"
    }

  // FULLY RANDOM ADAPTIVE CONTEXT SENSITIVITY
    def fullyRandomAdaptiveContextSensitiveAnalysis(prg: SchemeExp) = new AdaptiveModAnalysis(prg)
      with AdaptiveContextSensitivity
      with SelectRandom
      with TooManyRandom
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      {
        override def toString = "adaptive-context-sensitivity (random)"
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

    def charachteristicsAnalysis(prg: SchemeExp) = 
        new ModAnalysis[SchemeExp](prg)
            with StandardSchemeModFComponents
            with SchemeModFSemanticsM
            with SchemeModFNoSensitivity
            with BigStepModFSemantics
            with SymbolicSchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with CharacteristicsAnalysis:

            class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with IntraCharacteristicsAnalysis with BigStepModFIntra
            override def intraAnalysis(cmp: Component): AnalysisIntra =
                new AnalysisIntra(cmp)
