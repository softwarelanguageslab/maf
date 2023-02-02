package maf.modular.scheme.aam

import maf.modular.worklist.*
import maf.language.scheme.*
import maf.modular.scheme.*

class SchemeAAMAnalysis(prg: SchemeExp, k: Int)
    extends AAMScheme(prg)
    with SchemeConstantPropagationDomain
    with AAMCallSiteSensitivity(k)
    with FIFOWorklistAlgorithm[SchemeExp]

class SchemeAAMGCAnalysis(prg: SchemeExp, k: Int) extends SchemeAAMAnalysis(prg, k) with AAMAbstractCounting with AAMGC with AAMAnalysisResults
