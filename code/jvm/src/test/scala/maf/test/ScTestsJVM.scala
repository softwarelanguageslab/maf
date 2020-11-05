package maf.test

import maf.language.contracts.ScExp
import maf.language.contracts.ScLattice.Blame
import maf.modular.contracts.{ScSMTSolverJVM, ScSmtSolver}

trait ScTestsJVM extends ScTests {
  val primitivesMap = Map(
    ">"        -> ">/c",
    "="        -> "=/c",
    "<"        -> "</c",
    "string=?" -> "string=?/c",
    "int?"     -> "int?/c",
    "string?"  -> "string?/c",
    "nonzero?" -> "nonzero?/c",
    "any?"     -> "any?/c",
    "true?"    -> "true?/c",
    "false?"   -> "false?/c",
    "proc?"    -> "proc?/c"
  )

  trait ScAnalysisFixtureJVM extends ScAnalysisFixture {
    class ScTestAnalysisJVM(prog: ScExp) extends ScTestAnalysis(prog) {
      override def newSmtSolver(program: PC): ScSMTSolverJVM =
        new ScSMTSolverJVM(program, primitivesMap)
    }
  }

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

    /**
      * Asserts that the code should be verified as safe as long as the initial conditions
      * of the machine satisfy some predicate, otherwise asserts that the code should be unsafe
      */
    def safeIf(f: ScTestAnalysisJVM => Boolean): Unit

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
      if (refinements.nonEmpty) {
        this.command = s"(${this.command} ($value ${refinements.mkString(" ")}))"
      } else {
        this.command = s"(${this.command} $value)"
      }
      this
    }

    def analyse(f: ScTestAnalysisJVM => Unit): Unit = {
      val program = compile(command)
      val machine = new ScTestAnalysisJVM(program)
      machine.analyze()
      f(machine)
    }

    def safeIf(f: ScTestAnalysisJVM => Boolean): Unit = {
      val program = compile(command)
      val machine = new ScTestAnalysisJVM(program)
      if (f(machine)) {
        this.safe()
      } else {
        this.unsafe()
      }
    }

    def safe(): Unit = testName.should("be safe") in {
      analyse { machine =>
        machine.summary.blames shouldEqual Map()
      }
    }

    def unsafe(): Unit = testName.should("be unsafe") in {
      analyse { machine =>
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
    def safeIf(f: ScTestAnalysisJVM => Boolean): Unit                                       = ()
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
}
