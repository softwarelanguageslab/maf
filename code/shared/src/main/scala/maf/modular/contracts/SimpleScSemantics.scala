package maf.modular.contracts

import maf.language.contracts.ScExp
import maf.modular.ModAnalysis
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.util.benchmarks.Timeout

abstract class SimpleScSemantics(prg: ScExp)
    extends ModAnalysis(prg)
       with ScBigStepSemantics
       with ScBigStepSemanticsMonitored
       with ScStandardComponents
       with ScPrimitives
       with FIFOWorklistAlgorithm[ScExp] {

  val primitivesMap = Map(
    ">" -> ">/c",
    "=" -> "=/c",
    "<" -> "</c",
    "-" -> "-/c",
    "+" -> "+/c",
    "*" -> "*/c",
    "/" -> "//c",
    "string=?" -> "string=?/c",
    "int?" -> "int?/c",
    "string?" -> "string?/c",
    "nonzero?" -> "nonzero?/c",
    "any?" -> "any?/c",
    "true?" -> "true?/c",
    "false?" -> "false?/c",
    "proc?" -> "proc?/c",
    "bool?" -> "bool?/c",
    "and" -> "and/c",
    "or" -> "or/c",
    "not" -> "not/c",
    "number?" -> "int?/c",
    "char?" -> "char?/c",
    "pair?" -> "pair?/c",
    "string-length" -> "string-length",
    "null?" -> "null?/c"
  )

  override def analyze(timeout: Timeout.T): Unit = {
    setup()
    super.analyze(timeout)
  }

  override def intraAnalysis(component: Component) =
    new IntraAnalysis(component) with IntraScBigStepSemantics with IntraScBigStepSemanticsMonitored
}
