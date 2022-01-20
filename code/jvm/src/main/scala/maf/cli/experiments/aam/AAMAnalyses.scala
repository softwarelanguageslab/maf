package maf.cli.experiments.aam

import maf.aam.scv.*
import maf.aam.scheme.*
import maf.aam.{BaseSimpleWorklistSystem, SimpleWorklistSystem}
import maf.language.scheme.*
import maf.modular.scv.*
import maf.modular.scheme.*
import maf.cli.modular.scv.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.ContractScheme.*
import maf.core.*
import maf.aam.AAMAnalysis
import maf.aam.scheme.stores.SchemeImperativeStoreWidening

/**
 *   - CONF1: Function Boundaries
 *   - CONF2: No store allocated return
 *   - CONF3: Logging Global Store with function boundaries
 *   - CONF4: Logging Global Store without function boundaries
 *   - CONF5: Effect Driven Analysis (WIP)
 *   - CONF6: Logging Global Store with dependencies
 */
object AAMAnalyses:
    def aamBase(b: SchemeExp): AAMPeformanceMetrics =
      new BaseSchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with SchemeAAMLocalStore
        with SimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def aamConf1(b: SchemeExp): AAMPeformanceMetrics =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with SchemeFunctionCallBoundary
        with SchemeAAMLocalStore
        with SimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def aamConf2(b: SchemeExp): AAMPeformanceMetrics =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeFunctionCallBoundary
        with SchemeAAMLocalStore
        with SimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def aamConf3(b: SchemeExp): AAMPeformanceMetrics =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        //with SchemeAAMCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with SchemeFunctionCallBoundary
        with SchemeWideningAfterCondition
        with BaseSchemeLoggingLocalStore
        with BaseSimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def aamConf4(b: SchemeExp): AAMPeformanceMetrics =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with BaseSchemeLoggingLocalStore
        with BaseSimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def aamConf5(b: SchemeExp): AAMPeformanceMetrics =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        //with SchemeAAMCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with SchemeFunctionCallBoundary
        with SchemeWideningAfterCondition
        with SchemeImperativeStoreWidening
        //with BaseSchemeLoggingLocalStore
        //with BaseSimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def aamConf6(b: SchemeExp): AAMPeformanceMetrics =
      new SchemeAAMSemantics(b)
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        //with SchemeAAMCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMNoExt
        with SchemeStoreAllocateReturn
        with SchemeFunctionCallBoundary
        with SchemeWideningAfterCondition
        //with SchemeImperativeStoreWidening
        with BaseSchemeDependencyLoggingStore
        with BaseSimpleWorklistSystem
        with SchemeAAMAnalysisResults

    def scvAAMbase(b: SchemeExp): ScvAAMSemantics with AAMPeformanceMetrics =
      new ScvAAMSemantics(b)
        with BaseSchemeAAMSemantics
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeAAMLocalStore
        with SimpleWorklistSystem
        with SchemeAAMAnalysisResults {
        //with SchemeStoreAllocateReturn
        lazy val satSolver: ScvSatSolver[LatVal] =
            given lat: SchemeLattice[LatVal, Address] = lattice
            new JVMSatSolver
      }

    def scvAAMFnCallBoundaries(b: SchemeExp): ScvAAMSemantics with AAMPeformanceMetrics =
      new ScvAAMSemantics(b)
        with BaseSchemeAAMSemantics
        with AAMAnalysis
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        with SchemeFunctionCallBoundary
        //with SchemeAAMLocalStore
        with BaseSchemeLoggingLocalStore
        with BaseSimpleWorklistSystem
        with SchemeAAMAnalysisResults {
        //with SchemeStoreAllocateReturn
        lazy val satSolver: ScvSatSolver[LatVal] =
            given lat: SchemeLattice[LatVal, Address] = lattice
            new JVMSatSolver
      }
