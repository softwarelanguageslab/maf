package maf.modular.contracts

import maf.language.contracts.ScExp
import maf.modular.ModAnalysis
import maf.modular.worklist.FIFOWorklistAlgorithm

abstract class SimpleScSemantics(prg: ScExp)
    extends ModAnalysis(prg)
    with ScBigStepSemantics
    with ScBigStepSemanticsMonitored
    with ScStandardComponents
    with ScPrimitives
    with FIFOWorklistAlgorithm[ScExp] {

  val primitivesMap = Map(
    ">"        -> ">/c",
    "="        -> "=/c",
    "<"        -> "</c",
    "-"        -> "-/c",
    "+"        -> "+/c",
    "*"        -> "*/c",
    "/"        -> "//c",
    "string=?" -> "string=?/c",
    "int?"     -> "int?/c",
    "string?"  -> "string?/c",
    "nonzero?" -> "nonzero?/c",
    "any?"     -> "any?/c",
    "true?"    -> "true?/c",
    "false?"   -> "false?/c",
    "proc?"    -> "proc?/c",
    "bool?"    -> "bool?/c"
  )

  override def intraAnalysis(component: Component) = {
    setup()
    new IntraAnalysis(component) with IntraScBigStepSemantics with IntraScBigStepSemanticsMonitored
  }
}
