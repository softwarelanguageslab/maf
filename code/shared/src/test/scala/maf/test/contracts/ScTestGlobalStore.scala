package maf.test.contracts

import maf.language.contracts.ScExp
import maf.modular.contracts.ScGlobalStoreAnalysis
import maf.test.{ScAnalysisTests, ScTests}

trait ScTestGlobalStore extends ScTests with ScAnalysisTests {
  trait ScTestAnalysisGlobalStore extends ScTestAnalysis with ScGlobalStoreAnalysis
  override def newAnalysis(program: ScExp): ScTestAnalysisGlobalStore
}
