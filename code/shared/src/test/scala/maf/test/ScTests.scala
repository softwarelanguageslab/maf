package maf.test

import maf.core.Address
import maf.language.contracts.ScLattice.Blame
import maf.language.contracts._
import maf.modular.contracts.{
  ScCallInsensitivity,
  ScConstantPropagationDomain,
  ScSmtSolver,
  SimpleScSemantics
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait ScTests extends AnyFlatSpec with should.Matchers {
  trait ScLatticeFixture {
    import maf.lattice.ConstantPropagation._
    import L._
    val scCoPrLattice: ScCoProductLattice[I, B, Address]                 = new ScCoProductLattice[I, B, Address]()
    val scPrLattice: ScProductLattice[I, B, Address]                     = new ScProductLattice[I, B, Address]()
    val coLattice: ScLattice[scCoPrLattice.CoProductValue, Address]      = scCoPrLattice.isScLattice
    val proLattice: ScLattice[scPrLattice.ProductElements, Address]      = scPrLattice.isScLattice
    def one[V, A <: Address](implicit lattice: ScLattice[V, A]): V       = n(1)
    def n[V, A <: Address](v: Int)(implicit lattice: ScLattice[V, A]): V = lattice.injectInteger(v)
  }

  class ScTestAnalysis(prg: ScExp)
      extends SimpleScSemantics(prg)
      with ScCallInsensitivity
      with ScConstantPropagationDomain {

    val GLOBAL_STORE_ENABLED = false

    type SMTSolver = ScSmtSolver
    override def newSmtSolver(program: PC): SMTSolver =
      throw new Exception("no SMT solver found.")
  }

  protected def compile(exp: String): ScExp = {
    SCExpCompiler.preludedRead(exp)
  }

}

trait ScAnalysisTests extends ScTests {

  protected def newAnalysis(program: ScExp): ScTestAnalysis

  val primitivesMap = Map(
    ">"        -> ">/c",
    "="        -> "=/c",
    "<"        -> "</c",
    "-"        -> "-/c",
    "+"        -> "+/c",
    "*"        -> "*/c",
    "/"        -> "//c",
    "string=?" -> "string=?/c",
    "int?"     -> "int?/c",
    "string?"  -> "string?/c",
    "nonzero?" -> "nonzero?/c",
    "any?"     -> "any?/c",
    "true?"    -> "true?/c",
    "false?"   -> "false?/c",
    "proc?"    -> "proc?/c",
    "bool?"    -> "bool?/c"
  )

  trait VerifyTestBuilder extends ScLatticeFixture {
    def named(name: String): VerifyTestBuilder
    def applied0(): VerifyTestBuilder
    def applied(refinements: Set[String] = Set(), value: String = "OPQ"): VerifyTestBuilder
    def applied2(): VerifyTestBuilder

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
    def safeIf(f: ScTestAnalysis => Boolean): Unit

    def checked(f: (Set[Blame] => Unit)): Unit

    def analyse(f: ScTestAnalysis => Unit): Unit

    def tested(f: ScTestAnalysis => Unit): Unit

    /**
      * Ensures that all the annotated monitors are correctly validated.
      *
      * (Eg. all monitors marked as @unsafe are validated as @unsafe and all the ones that are marked as @safe are validated as such)
      */
    def full(): Unit
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

    def applied2(): VerifyTestBuilder = {
      this.command = s"(${this.command} OPQ OPQ)"
      this
    }

    def applied0(): VerifyTestBuilder = {
      this.command = s"(${this.command})"
      this
    }

    def analyse(f: ScTestAnalysis => Unit): Unit = {
      val program = compile(command)
      val machine = newAnalysis(program)
      machine.analyze()
      f(machine)
    }

    def safeIf(f: ScTestAnalysis => Boolean): Unit = {
      val program = compile(command)
      val machine = newAnalysis(program)
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

    def tested(f: ScTestAnalysis => Unit): Unit =
      testName.should("pass") in {
        analyse(f)
      }

    def checked(f: (Set[Blame] => Unit)): Unit = {
      testName.should("be checked") in {
        analyse(machine => f(machine.summary.blames.flatMap(_._2).toSet))
      }
    }

    def full(): Unit = {
      import maf.util.MonoidInstances._
      val program = compile(command)
      val monitors: List[ScAnnotated] = ScTraverser.traverse(program) {
        case m: ScMon if m.annotation.isDefined        => Right(List(m))
        case f: ScFunctionAp if f.annotation.isDefined => Right(List(f))
        case _                                         => Right(List())
      }

      testName.should("be fully checked") in {
        analyse(machine => {
          val blames = combineAll(machine.summary.blames.values.toList).map(_.blamedPosition.pos)
          monitors.foreach(
            m =>
              assert(
                (m.annotation.contains("@safe") && !blames
                  .contains(m.annotatedExpression.idn.pos)) ||
                  (m.annotation.contains("@unsafe") && blames.contains(
                    m.annotatedExpression.idn.pos
                  ))
              )
          )
        })
      }
    }
  }

  case object EmptyVerifyTestBuilder extends VerifyTestBuilder {
    def named(name: String): VerifyTestBuilder                                              = this
    def applied(refinements: Set[String] = Set(), value: String = "OPQ"): VerifyTestBuilder = this
    def applied2(): VerifyTestBuilder                                                       = this
    def applied0(): VerifyTestBuilder = this
    def safe(): Unit                                                                        = ()
    def unsafe(): Unit                                                                      = ()
    def safeIf(f: ScTestAnalysis => Boolean): Unit                                          = ()
    def checked(f: (Set[Blame] => Unit)): Unit                                              = ()
    def analyse(f: ScTestAnalysis => Unit): Unit                                            = ()
    def tested(f: ScTestAnalysis => Unit): Unit                                             = ()
    def full(): Unit                                                                        = ()
  }

  def verify(contract: String, expr: String): VerifyTestBuilder = {
    SomeVerifyTestBuilder(s"(mon $contract $expr)")
  }

  def eval(expr: String): VerifyTestBuilder = {
    SomeVerifyTestBuilder(expr)
  }

  def _verify(_contract: String, _expr: String): VerifyTestBuilder = EmptyVerifyTestBuilder

}
