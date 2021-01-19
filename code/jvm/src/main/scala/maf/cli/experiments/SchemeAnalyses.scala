package maf.cli.experiments

import maf.language.scheme._
import maf.modular._
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.scheme.modconc._
import maf.modular.worklist._

object SchemeAnalysesBoundedDomain {
  object NoSensitivity {
    def boundAnalysis(bnd: Int)(
      prg: SchemeExp
    ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeBoundedDomain with LIFOWorklistAlgorithm[SchemeExp] {
      lazy val bound = bnd
      override def toString() = "no-sensitivity"
    }
  }
  object CallSiteSensitivity {
    def boundAnalysis(bnd: Int)(
      prg: SchemeExp
    ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFCallSiteSensitivity with SchemeBoundedDomain with LIFOWorklistAlgorithm[SchemeExp] {
      lazy val bound = bnd
      override def toString() = "call-site-sensitivity"
    }
  }
}

object SchemeAnalyses {

  // Incremental analyses in maf.modular.incremental.scheme.SchemeAnalyses

  def contextInsensitiveAnalysis(
      prg: SchemeExp
    ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeConstantPropagationDomain with RandomWorklistAlgorithm[SchemeExp] {
    override def toString() = "no-sensitivity"
  }
  def callSiteContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
    with SchemeModFCallSiteSensitivity
    with SchemeConstantPropagationDomain
    with RandomWorklistAlgorithm[SchemeExp] {
    override def toString() = "call-site-sensitivity"
  }
  def kCFAAnalysis(prg: SchemeExp, kcfa: Int) = new SimpleSchemeModFAnalysis(prg)
    with SchemeModFKCallSiteSensitivity
    with SchemeConstantPropagationDomain
    with RandomWorklistAlgorithm[SchemeExp] {
    override def toString() = s"kCFA (k = $kcfa)"
    val k = kcfa
  }
  def fullArgContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
    with SchemeModFFullArgumentSensitivity
    with SchemeConstantPropagationDomain
    with RandomWorklistAlgorithm[SchemeExp] {
    override def toString() = "full-argument-sensitivity"
  }
  def adaptiveAnalysis(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)
    with AdaptiveSchemeModFSemantics
    with AdaptiveContextSensitivity
    with SchemeConstantPropagationDomain
    with RandomWorklistAlgorithm[SchemeExp] {
    val budget = k
    override def toString() = "adaptive-analysis"
  }
  def parallelKCFAAnalysis(
      prg: SchemeExp,
      n: Int,
      kcfa: Int
    ) = new ModAnalysis(prg)
    with SchemeModFSemantics
    with StandardSchemeModFComponents
    with BigStepModFSemantics
    with ParallelWorklistAlgorithm[SchemeExp]
    with SchemeModFKCallSiteSensitivity
    with SchemeConstantPropagationDomain {

    override def toString() = s"parallel k-CFA (n = $n ; k = $kcfa)"
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
    with ParallelWorklistAlgorithm[SchemeExp] {
    override def workers = n
    override def toString = s"parallel modconc (n = $n ; m = $m)"
    override def intraAnalysis(cmp: Component) = new SchemeModConcIntra(cmp) with ParallelIntra
    override def modFAnalysis(
        intra: SchemeModConcIntra
      ) = new InnerModFAnalysis(intra) with SchemeModFKCallSiteSensitivity with ParallelWorklistAlgorithm[SchemeExp] {
      val k = kcfa
      override def workers = m
      override def intraAnalysis(cmp: SchemeModFComponent) = new InnerModFIntra(cmp) with ParallelIntra
    }
  }
}
