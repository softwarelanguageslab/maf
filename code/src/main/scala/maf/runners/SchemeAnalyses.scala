package maf.runners

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.*
import maf.semantics.scheme.*
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.Preluded

object SchemeAnalyses:

    def modflocalAnalysis(prg: SchemeExp[Named] & Preluded, k: Int) =
        new SchemeModFLocal(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(k)
            with FIFOWorklistAlgorithm[SchemeExp[Named]]
            with SchemeModFLocalAnalysisResults

    def modflocalFSAnalysis(prg: SchemeExp[Named] & Preluded, k: Int, gc: Boolean = true) =
      new SchemeModFLocalFS(prg, gc)
          with SchemeConstantPropagationDomain
          with SchemeModFLocalCallSiteSensitivity(k)
          with FIFOWorklistAlgorithm[SchemeExp[Named]]
          with SchemeModFLocalFSAnalysisResults

    def modfADIAnalysis(prg: SchemeExp[Named] & Preluded, k: Int) =
      new SchemeModFADIAnalysis(prg, k)
          with SchemeModFADIAnalysisResults
