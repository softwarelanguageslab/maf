package maf.test.modular.contracts

import maf.language.contracts.ScLattice.Opq
import maf.modular.contracts.ScMain
import maf.test.ScTestsJVM

class ScBigStepSemantics extends ScTestsJVM {
  eval("(if OPQ 1 2)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.integerTop)
  }


  eval("(+ 1 1)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(2))
  }

  eval("(* 2 3)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(6))
  }

  eval("(/ 6 2)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(3))

  }

  eval("(- 3 3)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(0))
  }


  eval("((lambda (x) x) 1)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(1))
  }

  eval("((lambda (x) (if (= x 0) x 0)) 0)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(0))
  }

  eval("((lambda (x) (if (= x 0) x 1)) 0)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(0))
  }

  eval("((lambda (x) (if (= x 0) 1 2)) OPQ)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.integerTop)
  }


  eval("(int? 5)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("(int? OPQ)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.boolTop)
  }

  // should retain type information although the final value is top
  eval("(int? (if (= OPQ 2) 5 6))").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("(int? (if (= OPQ 2) 5 OPQ))").tested { machine =>
    // we don't know whether OPQ is an int or not
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.boolTop)
  }

  eval("(int? (if (< 2 5) 5 OPQ))").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }


  eval("(letrec (foo (lambda (x) (if (< x 1) 1 (+ (foo (+ x 1)) 1)))) (foo 42))").tested {
    // this is a test that checks whether the abstraction mechanism works, such that this infinite recursion
    // actually terminates in our analysis.
    machine =>
      machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.integerTop)
  }

  eval("(nonzero? 5)").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("(nonzero? 0)").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(false))
  }

  eval("(nonzero? OPQ)").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.boolTop)
  }

  eval("(proc? (lambda (x) x))").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("(proc? +)").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("(proc? (mon (~> any? any?) (lambda (x) x)))").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("(proc? 4)").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(false))
  }

  eval("(proc? OPQ)").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.boolTop)
  }

  eval("(dependent-contract? (~> any? any?))").tested { machine =>
    machine.summary.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  // an example of how the mon special form enriches the value it returns
  eval("(mon int? OPQ)").tested { machine =>
    "Opq{int?}"
    machine.lattice.getOpq(machine.summary.getReturnValue(ScMain).get) shouldEqual Set(
      Opq(Set("int?"))
    )
  }

  // an example of how the mon special form enriches the value it returns
  eval("(mon nonzero? (if (= 1 0) 0 1))").safe()


}
