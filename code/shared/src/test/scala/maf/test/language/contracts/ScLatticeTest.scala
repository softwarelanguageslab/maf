package maf.test.language.contracts

import maf.core.Address
import maf.language.contracts.ScLattice
import maf.language.contracts.ScLattice.Prim
import maf.test.ScTests

class ScLatticeTest extends ScTests {
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
