package maf.cli.experiments.precision

import maf.cli.experiments.SchemeAnalyses
import maf.cli.experiments.aam.AAMAnalyses
import maf.lattice.*
import maf.language.scheme.*

/**
 * This experiment compares the precision of
 *   - ScvModF: a modular analysis approach to soft contract verification.
 *   - AAMModF: an (classical) AAM approach optimized with functtion call boundaries and global stores
 */
object ScvPrecisionComparison
    extends AnalysisComparisonAlt[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:

    def analyses: List[(SchemeExp => Analysis, String)] = List(
      (SchemeAnalyses.scvModAnalysisWithRacketFeatures, "scv-modf"),
      (AAMAnalyses.scvAAMFnCallBoundaries, "scv-aam")
    )
