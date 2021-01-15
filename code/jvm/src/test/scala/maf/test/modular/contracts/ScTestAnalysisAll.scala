package maf.test.modular.contracts

import maf.test.{ScTestsJVMGlobalStore, ScTestsJVMLocalStore}
import maf.modular.contracts.ScMain

class ScTestAnalysisGlobalStoreTest extends ScTestsJVMGlobalStore with ScBigStepSemanticsTest
class ScTestAnalysisLocalStoreTest extends ScTestsJVMLocalStore with ScBigStepSemanticsTest

class ScSmallTestSuite extends ScTestsJVMGlobalStore {
  eval("""
    (define (list-of-test cc) (lambda (v) (letrec (loop (lambda (lst) (if (null? lst) #t (and (cc (car lst)) (loop (cdr lst)))))) (loop v)))) 

    ((list-of-test int?) '(1 2 #t))""").tested { machine =>
    machine.summary.returnValues.get(ScMain) shouldEqual Some(machine.lattice.boolTop)
  }
}
