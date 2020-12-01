package maf.test.modular.contracts

import maf.test.{ScTestsJVMGlobalStore, ScTestsJVMLocalStore}
import maf.modular.contracts.ScMain

class ScTestAnalysisGlobalStoreTest extends ScTestsJVMGlobalStore with ScBigStepSemanticsTest
class ScTestAnalysisLocalStoreTest  extends ScTestsJVMLocalStore with ScBigStepSemanticsTest

class ScSmallTestSuite extends ScTestsJVMLocalStore {
  eval("(+ 1 1)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(2))
  }

  verify("(~> any? int?)", "(lambda (x) (letrec (y (OPQ int?)) (if (int? x) x y)))")
    .applied()
    .safe()

  verify("(~> any? int?)", "(lambda (x) (letrec (y (OPQ int?)) (if (int? x) x x)))")
    .applied()
    .unsafe()

  verify("number?", "(or 5 2)").safe()

  /*
  eval("(define (f x) 5) (provide/contract (f (-> (or/c number? string?) number?))) (f 5)").tested {
    machine =>
      println(machine.summary.blames)
      assert(machine.summary.blames.isEmpty)
  }
   */

  eval("(define/contract (f x) (-> int? int?) x) (@unchecked f OPQ)").safe()

  eval("(define (f x) #t) (provide/contract (f (-> (or/c number? string?) number?))) (f 5)")
    .tested { machine =>
      assert(machine.summary.blames.nonEmpty)
    }
}
