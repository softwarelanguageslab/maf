package maf.cli.experiments

import maf.language.contracts.ScExp
import maf.modular.contracts._

object ScAnalyses {
  abstract class ScBaseAnalysis(prg: ScExp) extends SimpleScSemantics(prg) with ScJVMAnalysis

  def localStoreCallInsensitiveAnalysis(prg: ScExp): ScJVMAnalysis =
    new ScBaseAnalysis(prg) with ScCallInsensitivity with ScSchemeConstantPropagationDomain with ScLocalStoreAnalysis {
      override def toString = "sc_insensitive_local_store"
    }

  def globalStoreCallInsensitiveAnalysis(prg: ScExp): ScJVMAnalysis =
    new ScBaseAnalysis(prg) with ScCallInsensitivity with ScSchemeConstantPropagationDomain with ScGlobalStoreAnalysis {
      override def toString = "sc_insensitive_global_store"
    }
}
