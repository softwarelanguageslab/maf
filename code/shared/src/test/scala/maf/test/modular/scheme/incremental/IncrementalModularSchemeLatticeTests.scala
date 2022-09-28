package maf.test.modular.scheme.incremental

import maf.core.Position.Position
import maf.core.*
import maf.language.scheme.lattices.*
import maf.modular.incremental.IncrementalConfiguration
import maf.modular.incremental.scheme.lattice.*
import maf.modular.scheme.*
import maf.test.*
import org.scalatest.Tag
import org.scalatest.propspec.AnyPropSpec

trait IncrementalModularSchemeLatticeTests extends AnyPropSpec:

    lazy val testTags: Seq[Tag] = Seq(IncrementalTest, SlowTest)

    lazy val wrapper: IncrementalModularSchemeLattice[Address, _, _, _, _, _, _]
    private lazy val lattice = wrapper.incrementalSchemeLattice
    private lazy val adr1: VarAddr[Unit] = VarAddr(Identifier("adr1", NoCodeIdentity), ())
    private lazy val adr2: VarAddr[Unit] = VarAddr(Identifier("adr2", NoCodeIdentity), ())

    private lazy val num1 = lattice.number(1)
    private lazy val num2 = lattice.number(2)
    private lazy val rea1 = lattice.real(1)
    private lazy val rea2 = lattice.real(2)
    private lazy val str1 = lattice.string("abc")
    private lazy val str2 = lattice.string("def")
    private lazy val cha1 = lattice.char('a')
    private lazy val cha2 = lattice.char('b')
    private lazy val boo1 = lattice.bool(true)
    private lazy val boo2 = lattice.bool(false)
    private lazy val prm1 = lattice.primitive("gcd")
    private lazy val prm2 = lattice.primitive("lcm")
    private lazy val sym1 = lattice.symbol("a")
    private lazy val sym2 = lattice.symbol("b")
    private lazy val cns1 = lattice.cons(num1, boo1)
    private lazy val cns2 = lattice.cons(num2, boo2)
    private lazy val poi1 = lattice.pointer(adr1)
    private lazy val poi2 = lattice.pointer(adr2)

    private lazy val values = List(num1, num2, rea1, rea2, str1, str2, cha1, cha2, boo1, boo2, prm1, prm2, sym1, sym2, cns1, cns2, poi1, poi2)

    lazy val origLattice = wrapper.schemeLattice

    // Test lattice operations.
    SchemeOp.unaryOperators.foreach { op =>
        property(s"Unary operation $op preserves address annotations and result.", testTags: _*) {
            values.foreach { v =>
                val res1 = lattice.op(op)(List(lattice.addAddress(v, adr1)))
                val res2 = origLattice.op(op)(List(v.toL()))
                for {
                    r1 <- res1
                    r2 <- res2
                    _ = assert(lattice.getAddresses(r1) == Set(adr1),
                               s"Annotation set comparison for unary operation $op failed (found: ${lattice.getAddresses(r1)})."
                    )
                    _ = assert(r1.toL() == r2, s"Annotation set comparison for unary operation $op failed (found: ${r1.toL()} using $v).")
                } yield ()
            }
        }
    }
    SchemeOp.binaryOperators.foreach { op =>
        property(s"Binary operation $op preserves address annotations and result.", testTags: _*) {
            values.foreach { v1 =>
                values.foreach { v2 =>
                    val res1 = lattice.op(op)(List(lattice.addAddress(v1, adr1), lattice.addAddress(v2, adr2)))
                    val res2 = origLattice.op(op)(List(v1.toL(), v2.toL()))
                    for {
                        r1 <- res1
                        r2 <- res2
                        _ = assert(lattice.getAddresses(r1) == Set(adr1, adr2),
                                   s"Annotation set comparison for binary operation $op failed (found: ${lattice.getAddresses(r1)})."
                        )
                        _ = assert(r1.toL() == r2)
                    } yield ()
                }
            }
        }
    }

    // Test join.
    private val vs = List(num1, rea1, str1, cha1, boo1, prm1, sym1, cns1, poi1) // Some reduction to avoid duplicate test names.
    vs.foreach { v1 =>
        vs.filterNot(_.toString == v1.toString).foreach { v2 =>
            property(s"Join of $v1 and $v2 is correct.", testTags: _*) {
                val av1 = lattice.addAddress(v1, adr1)
                val av2 = lattice.addAddress(v2, adr2)
                val j1 = lattice.join(av1, av2)
                val j2 = origLattice.join(v1.toL(), v2.toL())
                assert(j1.toL() == j2, s"Value join is computed incorrectly for $v1 and $v2.")
                assert(lattice.getAddresses(j1) == Set(adr1, adr2), s"Value join does not correctly retain addresses for $v1 and $v2.")
            }
        }
    }

end IncrementalModularSchemeLatticeTests

class IncrementalModularSchemeTypeLatticeTests extends IncrementalModularSchemeLatticeTests:
    lazy val wrapper = IncrementalSchemeTypeDomain.modularLattice
end IncrementalModularSchemeTypeLatticeTests

class IncrementalModularSchemeCPLatticeTests extends IncrementalModularSchemeLatticeTests:
    lazy val wrapper = IncrementalSchemeConstantPropagationDomain.modularLattice
end IncrementalModularSchemeCPLatticeTests
