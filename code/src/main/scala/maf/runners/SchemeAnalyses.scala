package maf.runners

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.semantics.scheme.*

object SchemeAnalyses:

    def modflocalAnalysis(k: Int) =
        new SchemeModFLocal
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(k)
            with SchemeModFLocalAnalysisResults

    def modflocalFSAnalysis(k: Int, gc: Boolean = true) =
      new SchemeModFLocalFS(gc)
          with SchemeConstantPropagationDomain
          with SchemeModFLocalCallSiteSensitivity(k)
          with SchemeModFLocalFSAnalysisResults

    def modfADIAnalysis(k: Int) =
      new SchemeModFADIAnalysis(k)
          with SchemeModFADIAnalysisResults
