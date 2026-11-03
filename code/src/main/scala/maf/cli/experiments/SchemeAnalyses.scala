package maf.cli.experiments

import maf.language.scheme._
import maf.modular.scheme._
import maf.modular.scheme.modflocal._
import maf.modular.worklist._

object SchemeAnalyses:

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
