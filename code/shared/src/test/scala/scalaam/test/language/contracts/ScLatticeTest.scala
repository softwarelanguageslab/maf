package scalaam.test.language.contracts

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scalaam.core.Address
import scalaam.language.contracts.ScLattice.Prim
import scalaam.language.contracts.{ScCoProductLattice, ScLattice, ScProductLattice}

class ScLatticeTest extends AnyFlatSpec with should.Matchers {
  trait ScLatticeFixture {
    import scalaam.lattice.ConstantPropagation._
    import L._
    val scCoPrLattice: ScCoProductLattice[I, B, Address]                 = new ScCoProductLattice[I, B, Address]()
    val scPrLattice: ScProductLattice[I, B, Address]                     = new ScProductLattice[I, B, Address]()
    val coLattice: ScLattice[scCoPrLattice.CoProductValue, Address]      = scCoPrLattice.isScLattice
    val proLattice: ScLattice[scPrLattice.ProductElements, Address]      = scPrLattice.isScLattice
    def one[V, A <: Address](implicit lattice: ScLattice[V, A]): V       = n(1)
    def n[V, A <: Address](v: Int)(implicit lattice: ScLattice[V, A]): V = lattice.injectInteger(v)
    def withLattice[V, A <: Address](implicit lattice: ScLattice[V, A]): Unit
  }

  "Arithmetic operations" should "preserve bottom" in new ScLatticeFixture {
    def withLattice[V, A <: Address](implicit lattice: ScLattice[V, A]): Unit = {
      Seq("*", "+", "-").foreach { op =>
        lattice.applyPrimitive(Prim(op))(lattice.bottom, one) shouldEqual lattice.bottom
      }
    }

    withLattice(coLattice)
    //withLattice(proLattice)
  }

  "Arithmetic operations" should "constant propagate" in new ScLatticeFixture {
    def withLattice[V, A <: Address](implicit lattice: ScLattice[V, A]): Unit = {
      lattice.applyPrimitive(Prim("+"))(n(5), n(2)) shouldEqual n(7)
      lattice.applyPrimitive(Prim("+"))(n(5), n(2)) should not be n(9)
      lattice.applyPrimitive(Prim("*"))(n(5), n(2)) should not be n(11)
      lattice.applyPrimitive(Prim("*"))(n(5), n(2)) shouldEqual n(10)
    }

    withLattice(coLattice)
    //withLattice(proLattice)
  }

  "Arithmetic operations in the coproduct lattice" should "fail on values of incompatible type" in new ScLatticeFixture {
    def withLattice[V, A <: Address](implicit lattice: ScLattice[V, A]): Unit = {
      Seq("*", "+", "-").foreach { op =>
        lattice.applyPrimitive(Prim(op))(n(5), lattice.injectBoolean(true)) shouldEqual lattice.bottom
      }
    }

    withLattice(coLattice)
  }
}
