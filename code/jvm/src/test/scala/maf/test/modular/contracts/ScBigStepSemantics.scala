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

  /**
    * Test that the OPQ value is refined to an integer
    */
  eval("(int? (mon int? OPQ))").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  /**
    * Test if the semantics when running on a valid value are the same as the wrapped value
    */
  eval("((flat int?) 0)").tested { machine =>
    println(machine.getReturnValue(ScMain))
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  eval("((flat int?) OPQ)").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.boolTop)
  }

  eval("(letrec (zero? (flat (lambda (x) (= x 0)))) (mon zero? 1))").tested { machine =>
    println(machine)
  }

  /**
    * An integer literal should always pass the `int?` test
    */
  verify("int?", "5").named("flat_lit_int?").safe()

  /**
    * An opaque value can be different from an integer, so this should not be verified as safe
    */
  verify("int?", "OPQ").named("flat_OPQ_int?").unsafe()

  /**
    * A contract from any to any should always be verified as safe
    */
  verify("(~> any? any?)", "(lambda (x) x)").applied().named("id_any2any").safe()

  verify("(~> any? int?)", "(lambda (x) 1)").applied().named("any2int_constant1").safe()

  verify("(~> any? int?)", "(lambda (x) OPQ)").applied().named("any2int_opq").unsafe()

  verify("(~> int? any?)", "(lambda (x) x)")
    .applied()
    .unsafe()

  verify("(~> nonzero? any?)", "(lambda (x) x)")
    .applied(value = "0")
    .unsafe()

  verify("(~> nonzero? any?)", "(lambda (x) x)")
    .applied(value = "1")
    .safe()

  // With the input of 1 this recursive function can return a zero number
  verify(
    "(~> any? nonzero?)",
    "(letrec (foo (lambda (x) (if (= x 1) 1 (- (foo (- x 1)) 1)))) foo)"
  ).applied()
    .unsafe()

  // This is the same as above, but the abstract interpretation keeps information about the type of the value.
  // in this case the value is Number(top) which is sufficient for int? to succeed
  verify(
    "(~> any? int?)",
    "(letrec (foo (lambda (x) (if (= x 0) 1 (- (foo (- x 1)) 1)))) foo)"
  ).applied()
    .safe()

  // because we are using symbolic values with refinement sets, it should generate a blame on the domain contract
  // but not on the range contract
  verify("(~> int? int?)", "(lambda (x) x)").applied().checked { blames =>
    blames.size shouldEqual 1
  }

  verify("(~> int? int?)", "(lambda (x) OPQ)").tested { machine =>
  }

  verify("(~> any? nonzero?)", "(lambda (x) (if (< x 2) (if (> x 2) 0 2) 2))").applied().safe()
  verify("(~> any? nonzero?)", "(lambda (x) (if (< x 2) (if (< x 2) 0 2) 2))").applied().unsafe()

  verify("(~> (~> any? int?) int?)", "(lambda (g) (g OPQ))").applied().unsafe()

  verify("(~> int? int?)", "(lambda (x) (letrec (y x) (begin (set! x #t) y)))")
    .applied(Set("int?"))
    .unsafe()

  /**
    * This should not be verified as safe, as the applied lambda changes x before returning it, this is to
    * test whether the store cache is correctly invalidated if the applied lambda captures variables
    */
  verify("(~> int? int?)", "(lambda (x) (begin ((lambda (y) (set! x #t)) OPQ) x))")
    .applied(Set("int?"))
    .unsafe()

  verify(
    "(~> int? int?)",
    "(lambda (x) (begin (assume (x int?) ((lambda (y) (set! x #t)) OPQ)) x))"
  ).applied(Set("int?"))
    .safe()

  verify("(~> any? int?)", "(lambda (x) (letrec (y (OPQ int?)) (if (int? x) x y)))")
    .applied()
    .safe()

  verify("(~> any? int?)", "(lambda (x) (letrec (y (OPQ int?)) (if (int? x) x x)))")
    .applied()
    .unsafe()
}
