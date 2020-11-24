package maf.test.contracts

import maf.language.contracts.ScExp
import maf.modular.contracts.{ScGlobalStoreAnalysis, ScLocalStoreAnalysis}
import maf.test.{ScAnalysisTests, ScTests}

trait ScTestLocalStore extends ScTests with ScAnalysisTests {
  trait ScTestAnalysisLocalStore extends ScTestAnalysis with ScLocalStoreAnalysis
  override def newAnalysis(program: ScExp): ScTestAnalysisLocalStore
}
