package maf.test.contracts

import maf.language.contracts.ScExp
import maf.test.{ScAnalysisTests, ScTests}

trait ScTestGlobalStore extends ScTests with ScAnalysisTests {
  trait ScTestAnalysisGlobalStore extends ScTestAnalysis {
    override val GLOBAL_STORE_ENABLED: Boolean = true
  }

  override def newAnalysis(program: ScExp): ScTestAnalysisGlobalStore
}
