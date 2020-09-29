package scalaam.test.modular.contracts

import scalaam.language.contracts.ScLattice.{Blame, Opq, Prim}
import scalaam.modular.contracts.ScMain
import scalaam.test.ScTestsJVM

class ScEvalSuite extends ScTestsJVM {
  trait VerifyTestBuilder extends ScLatticeFixture with ScAnalysisFixtureJVM {
    def named(name: String): VerifyTestBuilder
    def applied(refinements: Set[String] = Set(), value: String = "OPQ"): VerifyTestBuilder

    /**
      * Generates a test that asserts that the result of the verification contains no blames
      */
    def safe(): Unit

    /**
      * Generates a test that asserts that the result of the verification contains a blame
      */
    def unsafe(): Unit

    def checked(f: (Set[Blame] => Unit)): Unit

    def analyse(f: ScTestAnalysisJVM => Unit): Unit

    def tested(f: ScTestAnalysisJVM => Unit): Unit
  }

  case class SomeVerifyTestBuilder(var command: String) extends VerifyTestBuilder {
    private var name: String = ""
    def named(name: String): VerifyTestBuilder = {
      this.name = name
      this
    }

    def testName: String = if (name.isEmpty) command else name

    def applied(refinements: Set[String] = Set(), value: String = "OPQ"): VerifyTestBuilder = {
      this.command = s"(${this.command} $value)"
      this
    }

    def analyse(f: ScTestAnalysisJVM => Unit): Unit = {
      val program = compile(command)
      val machine = new ScTestAnalysisJVM(program)
      machine.analyze()
      f(machine)
    }

    def safe(): Unit = analyse { machine =>
      testName.should("be safe") in {
        machine.summary.blames shouldEqual Map()
      }
    }

    def unsafe(): Unit = analyse { machine =>
      testName.should("be unsafe") in {
        assert(machine.summary.blames.nonEmpty)
        println("Not verified!")
        println(command)
        val blames = machine.summary.blames.values.flatMap(_.map(_.blamedPosition.pos.col)).toList
        for (col <- 0 to command.length) {
          if (blames.contains(col + 1)) {
            print("^")
          } else {
            print(" ")
          }
        }
        println()
        //println((" " * (machine.summary.blames.values.head.head.blamedPosition.pos.col - 1)) ++ "^")
      }
    }

    def tested(f: ScTestAnalysisJVM => Unit): Unit =
      testName.should("pass") in {
        analyse(f)
      }

    def checked(f: (Set[Blame] => Unit)): Unit = {
      testName.should("be checked") in {
        analyse(machine => f(machine.summary.blames.flatMap(_._2).toSet))
      }
    }
  }

  case object EmptyVerifyTestBuilder extends VerifyTestBuilder {
    def named(name: String): VerifyTestBuilder                                              = this
    def applied(refinements: Set[String] = Set(), value: String = "OPQ"): VerifyTestBuilder = this
    def safe(): Unit                                                                        = ()
    def unsafe(): Unit                                                                      = ()
    def checked(f: (Set[Blame] => Unit)): Unit                                              = ()
    def analyse(f: ScTestAnalysisJVM => Unit): Unit                                         = ()
    def tested(f: ScTestAnalysisJVM => Unit): Unit                                          = ()
  }

  def verify(contract: String, expr: String): VerifyTestBuilder = {
    SomeVerifyTestBuilder(s"(mon $contract $expr)")
  }

  def eval(expr: String): VerifyTestBuilder = {
    SomeVerifyTestBuilder(expr)
  }

  def _verify(_contract: String, _expr: String): VerifyTestBuilder = EmptyVerifyTestBuilder

  eval("1").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectInteger(1))
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

  eval("(int? (mon int? OPQ))").tested { machine =>
    machine.getReturnValue(ScMain) shouldEqual Some(machine.lattice.injectBoolean(true))
  }

  // An integer literal should always pass the `int?` test
  verify("int?", "5").named("flat_lit_int?").safe()

  // An opaque value can be different from an integer, so this should not be verified as safe
  verify("int?", "OPQ").named("flat_OPQ_int?").unsafe()

  // A contract from any to any should always be verified as safe
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

  verify("(~> int? int?)", "(lambda (x) OPQ)").applied().unsafe()

  verify("(~> any? nonzero?)", "(lambda (x) (if (< x 2) (if (> x 2) 0 1) 2))").applied().safe()
  verify("(~> any? nonzero?)", "(lambda (x) (if (< x 2) (if (< x 2) 0 1) 2))").applied().unsafe()

  eval("(letrec (int?/c (lambda (x) (check int? x))) (mon int?/c OPQ))").tested { machine =>
    println(machine.unverified)
  }

  eval("(letrec (int?/c (lambda (x) (check int? x))) (mon int?/c 5))").tested { machine =>
    println(machine.unverified)
  }

  eval("""
      |(letrec 
      |  (and/c (lambda (c1 c2) (lambda (x) (and (check c1 x) (check c2 x)))))
      |  (mon (and/c int? nonzero?) 0))
      |""".stripMargin).tested { machine =>
    println(machine.unverified)
    machine.unverified.values.toList shouldEqual (List(
      machine.Partially(
        Set(machine.lattice.injectPrim(Prim("nonzero?")))
      )
    ))
  }

  eval("""
         |(letrec 
         |  (and/c (lambda (c1 c2) (lambda (x) (and (check c1 x) (check c2 x)))))
         |  (mon (and/c (lambda (x) (> x 2)) nonzero?) 0))
         |""".stripMargin).tested { machine =>
    println(machine.unverified)
  }

  eval("""(letrec (n 0) 
      |  (letrec (min (lambda (y) (if (< y n) y n)))
      |     ((mon (~ int? (lambda (n) (lambda (a) (=< a n))))
      |         (lambda (x) 
      |          (begin 
      |             (set! n (min x))
      |             n))) OPQ)))""".stripMargin).unsafe()

  _verify(
    "(~ int? (lambda (x) (lambda (y) (=< y x))))",
    """
    |(((lambda (min)
    |   (lambda (n)
    |     (lambda (x)
    |       (begin
    |         (set! n (min n x))
    |         n))))
    |   (lambda (a b)
    |      (if (=< a b)
    |         a
    |         b)))
    |   0)
    |""".stripMargin
  ).applied().unsafe()

  // this should be verified as safe, as the opaque value will be refined to a an even? opaque value
  _verify("(int? ~ even?)", "(lambda (x) (* x 2))")
    .applied(Set("int?"))
    .named("int2even_timestwo")
    .safe()

  // The example below can only be verified if we extend our constant propagation lattice with sign information
  // the result value will always be top
  _verify("(int? ~ nonzero?)", "(letrec (foo (lambda (x) (if (= x 0) 1 (+ (foo (- x 1)) 1)))) foo)")
    .applied()
    .unsafe()
}
