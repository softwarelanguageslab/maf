package maf.test.modular.contracts

import maf.test.{ScTestsJVMGlobalStore, ScTestsJVMLocalStore}
import maf.modular.contracts.ScMain

class ScTestAnalysisGlobalStoreTest extends ScTestsJVMGlobalStore with ScBigStepSemanticsTest
class ScTestAnalysisLocalStoreTest  extends ScTestsJVMLocalStore with ScBigStepSemanticsTest

class ScSmallTestSuite extends ScTestsJVMLocalStore {
  eval("(+ 1 1)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(2))
  }
}
