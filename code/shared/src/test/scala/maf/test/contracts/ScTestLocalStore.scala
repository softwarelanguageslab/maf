package maf.test.contracts

import maf.language.contracts.ScExp
import maf.test.{ScAnalysisTests, ScTests}

trait ScTestLocalStore extends ScTests with ScAnalysisTests {
  trait ScTestAnalysisLocalStore extends ScTestAnalysis {
    override val GLOBAL_STORE_ENABLED: Boolean = false
  }

  override def newAnalysis(program: ScExp): ScTestAnalysisLocalStore
}
