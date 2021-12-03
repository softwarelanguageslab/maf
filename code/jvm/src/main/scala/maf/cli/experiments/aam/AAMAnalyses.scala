package maf.cli.experiments.aam

import maf.aam.scv.*
import maf.aam.scheme.*
import maf.aam.*
import maf.language.scheme.*
import maf.modular.scv.*
import maf.modular.scheme.*
import maf.cli.modular.scv.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.ContractScheme.*
import maf.core.*
import maf.aam.AAMAnalysis

object AAMAnalyses:
    def aamBase(b: SchemeExp): AAMAnalysis =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMAnalysisResults
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn

    def aamBaseFnBoundaries(b: SchemeExp): AAMAnalysis =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMAnalysisResults
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with SchemeFunctionCallBoundary

    def scvAAMbase(b: SchemeExp): ScvAAMSemantics =
      new ScvAAMSemantics(b)
        with BaseSchemeAAMSemantics
        with AAMAnalysis
        with SchemeAAMAnalysisResults
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain {
        //with SchemeStoreAllocateReturn
        lazy val satSolver: ScvSatSolver[LatVal] =
            given lat: SchemeLattice[LatVal, Address] = lattice
            new JVMSatSolver
      }

    def scvAAMFnCallBoundaries(b: SchemeExp): ScvAAMSemantics =
      new ScvAAMSemantics(b)
        with BaseSchemeAAMSemantics
        with AAMAnalysis
        with SchemeAAMAnalysisResults
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeFunctionCallBoundary {
        //with SchemeStoreAllocateReturn
        lazy val satSolver: ScvSatSolver[LatVal] =
            given lat: SchemeLattice[LatVal, Address] = lattice
            new JVMSatSolver
      }
