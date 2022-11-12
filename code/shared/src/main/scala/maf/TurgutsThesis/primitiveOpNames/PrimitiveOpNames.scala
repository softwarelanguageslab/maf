package maf.TurgutsThesis.primitiveOpNames

import maf.core.NoCodeIdentity
import maf.language.scheme.{SchemeBegin, SchemeExp}
import maf.language.scheme.primitives.SchemeLatticePrimitives
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modflocal.{SchemeModFLocal, SchemeModFLocalAdaptiveWideningPolicyA, SchemeModFLocalAnalysisResults, SchemeModFLocalNoSensitivity}
import maf.modular.worklist.FIFOWorklistAlgorithm

object PrimitiveOpNames:
  
  /** private definition for information hiding */
  private def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with SchemeModFLocalAnalysisResults
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAdaptiveWideningPolicyA(1)

  /** Easy/efficient way of getting all primitive names */
  val allNames: List[String] =
    analysis(SchemeBegin(List(), NoCodeIdentity)).primitives.allPrimitives.keySet.toList
