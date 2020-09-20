package scalaam.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scalaam.core.{Address, Environment, Position}
import scalaam.language.contracts._
import scalaam.language.sexp.SExpParser
import scalaam.modular.adaptive.AdaptiveGlobalStore
import scalaam.modular.contracts.{
  Call,
  ScCallInsensitivity,
  ScConstantPropagationDomain,
  ScGenericAddr,
  ScSmallStepSemantics,
  ScSmtSolver,
  ScVarAddr,
  SimpleScSemantics
}
import scalaam.modular.{FIFOWorklistAlgorithm, GlobalStore}

trait ScTests extends AnyFlatSpec with should.Matchers {
  trait ScLatticeFixture {
    import scalaam.lattice.ConstantPropagation._
    import L._
    val scCoPrLattice: ScCoProductLattice[I, B, Address]                 = new ScCoProductLattice[I, B, Address]()
    val scPrLattice: ScProductLattice[I, B, Address]                     = new ScProductLattice[I, B, Address]()
    val coLattice: ScLattice[scCoPrLattice.CoProductValue, Address]      = scCoPrLattice.isScLattice
    val proLattice: ScLattice[scPrLattice.ProductElements, Address]      = scPrLattice.isScLattice
    def one[V, A <: Address](implicit lattice: ScLattice[V, A]): V       = n(1)
    def n[V, A <: Address](v: Int)(implicit lattice: ScLattice[V, A]): V = lattice.injectInteger(v)
  }

  trait ScAnalysisFixture extends ScLatticeFixture {
    class ScTestAnalysis(prg: ScExp)
        extends SimpleScSemantics(prg)
        with ScCallInsensitivity
        with ScConstantPropagationDomain {

      type SMTSolver = ScSmtSolver
      override def newSmtSolver(program: PC): SMTSolver =
        throw new Exception("no SMT solver found.")
    }
  }

  protected def compile(exp: String): ScExp = {
    val sexp = SExpParser.parse(exp)
    SCExpCompiler.compile(sexp.head)
  }
}
