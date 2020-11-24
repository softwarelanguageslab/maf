package maf.cli.experiments

import maf.language.contracts.ScExp
import maf.modular.contracts._

object ScAnalyses {
  abstract class ScBaseAnalysis(prg: ScExp) extends SimpleScSemantics(prg) with ScJVMAnalysis

  def localStoreCallInsensitiveAnalysis(prg: ScExp): ScJVMAnalysis =
    new ScBaseAnalysis(prg) with ScCallInsensitivity with ScConstantPropagationDomain
    with ScLocalStoreAnalysis

  def globalStoreCallInsensitiveAnalysis(prg: ScExp): ScJVMAnalysis =
    new ScBaseAnalysis(prg) with ScCallInsensitivity with ScConstantPropagationDomain
    with ScGlobalStoreAnalysis
}
