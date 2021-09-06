package maf.test.language.scheme

import org.scalacheck.Prop._
import maf.core._
import maf.language.scheme.lattices._
import maf.test.lattice._

// inherits the standard lattice tests from `LatticeTest`
class SchemeLatticeTests[L](gen: SchemeLatticeGenerator[L])(implicit val schemeLattice: SchemeLattice[L, _]) extends LatticeTest(gen):
    // because of higher entropy in Scheme lattice values, verify each property with more examples!
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
      PropertyCheckConfiguration(minSuccessful = 1000)
    val schemeLaws = newProperties("Scheme") { p =>
        implicit val arb = gen.anyArb
        implicit val shr = gen.shrink
        implicit val auo = gen.arbUnop
        implicit val abo = gen.arbBinop
        implicit val ato = gen.arbTernop

        def inject(v: Any) = schemeLattice.Injector.inject(v)

        import schemeLattice._
        /* */
        def convert(mf: MayFail[L, Error]): L = mf match
            case MayFailSuccess(v) => v
            case MayFailBoth(v, _) => v
            case MayFailError(_)   => bottom
        p.property("⊥ ⊑ ⊥") = subsumes(bottom, bottom)
        p.property("∀ x: ⊥ ⊑ x") = forAll((x: L) => subsumes(x, bottom))
        p.property("∀ x: join(x, ⊥) = join(⊥, x) = x") = forAll((x: L) => join(x, bottom) == x && join(bottom, x) == x)
        p.property("∀ x, y: y ⊑ x ⇒ join(x, y) = x") = forAll { (x: L) =>
          forAll(gen.le(x)) { (y: L) =>
            join(x, y) == x
          }
        }
        p.property("∀ x, y: x ⊑ join(x, y), y ⊑ join(x, y)") = forAll { (x: L, y: L) =>
            val xy = join(x, y)
            subsumes(xy, x) && subsumes(xy, y)
        }
        p.property("∀ x, y, z: x ⊑ join(x, join(y, z)), y ⊑ join(x, join(y, z)), z ⊑ join(x, join(y, z))") = forAll { (x: L, y: L, z: L) =>
            val xyz = join(x, join(y, z))
            subsumes(xyz, x) && subsumes(xyz, y) && subsumes(xyz, z)
        }
        p.property("injection preserves truth") = isTrue(inject(true)) && isFalse(inject(false))
        p.property("!isTrue(⊥) && !isFalse(⊥)") = !isTrue(bottom) && !isFalse(bottom)
        p.property("isTrue(BoolTop) && isFalse(BoolTop)") =
            val boolTop = join(inject(true), inject(false))
            isTrue(boolTop) && isFalse(boolTop)
        /* Unary operators preserve bottom */
        p.property("∀ unop: unop(⊥) = ⊥") = forAll((unop: SchemeOp.SchemeOp1) => convert(op(unop)(List(bottom))) == bottom)
        /* Unary operators are monotone */
        /* TODO: fails because of NaN
     p.property("∀ unop, a, b: a ⊑ b ⇒ unop(a) ⊑ unop(b)") = forAll { (unop: SchemeOp.SchemeOp1, b: L) =>
            forAll(gen.le(b)) { (a: L) =>
                val fa = convert(op(unop)(List(a)))
                val fb = convert(op(unop)(List(b)))
                println(s"f = $unop")
                println(s"f($a) = $fa")
                println(s"f($b) = $fb")
                subsumes(b, a) ==> subsumes(fb, fa)
            }
        } */
        /* Binary operators preverse bottom */
        p.property("∀ binop, a: binop(⊥,a) = ⊥ = binop(a,⊥)") = forAll((binop: SchemeOp.SchemeOp2, a: L) =>
          if binop == SchemeOp.MakeVector then {
            /* MakeVector is a strange beast as it accepts bottom as a second argument to create an uninitialized vector */
            true
          } else {
            convert(op(binop)(List(bottom, a))) == bottom &&
            convert(op(binop)(List(a, bottom))) == bottom
          }
        )

        /* Ternary operators preserve bottom */
        p.property("∀ ternop, a: ternop(⊥,a,b) = ⊥ = ternop(a,⊥,b) = ternop(a,b,⊥)") = forAll((ternop: SchemeOp.SchemeOp3, a: L, b: L) =>
          convert(op(ternop)(List(bottom, a, b))) == bottom &&
            convert(op(ternop)(List(a, bottom, b))) == bottom &&
            convert(op(ternop)(List(a, b, bottom))) == bottom
        )

        /* not is correct */
        p.property("isFalse(not(true)) && isTrue(not(false)") =
          isFalse(convert(op(SchemeOp.Not)(List(inject(true))))) && isTrue(convert(op(SchemeOp.Not)(List(inject(false)))))

        /* and is correct */
        p.property("∀ b1, b2: b1 && b2 = and(inject(b1), inject(b2))") = forAll { (b1: Boolean, b2: Boolean) =>
            val v1 = inject(b1)
            val v2 = inject(b2)
            if b1 && b2 then isTrue(and(v1, v2)) else isFalse(and(v1, v2))
        }

        /* or is correct */
        p.property("∀ b1, b2: b1 || b2 = or(inject(b1), inject(b2))") = forAll { (b1: Boolean, b2: Boolean) =>
            val v1 = inject(b1)
            val v2 = inject(b2)
            if b1 || b2 then isTrue(or(v1, v2)) else isFalse(or(v1, v2))
        }

        /* lt is correct */
        p.property("∀ n1, n2: n1 < n2 = lt(inject(n1), inject(n2))") = forAll { (n1: Int, n2: Int) =>
            val v1 = inject(n1)
            val v2 = inject(n2)
            if n1 < n2 then isTrue(convert(op(SchemeOp.Lt)(List(v1, v2))))
            else isFalse(convert(op(SchemeOp.Lt)(List(v1, v2))))
        }

        /* eq is correct */
        p.property("∀ n1, n2: n1 == n2 = eq(inject(n1), inject(n2))") = forAll { (n1: Int, n2: Int) =>
            val v1 = inject(n1)
            val v2 = inject(n2)
            if n1 == n2 then isTrue(convert(op(SchemeOp.NumEq)(List(v1, v2))))
            else isFalse(convert(op(SchemeOp.NumEq)(List(v1, v2))))
        }

        /* TODO: properties that match the specification of other SchemeOps */

        /* Properties about vectors */
        p.property("∀ vct, idx1, val1, idx2, val2: vectorSet(vectorSet(vct,idx1,val1),idx2,val2) = vectorSet(vectorSet(vct,idx2,val2),idx1,val1)") =
          forAll(gen.anyVec, gen.anyInt, gen.any, gen.anyInt, gen.any) { (vct: L, idx1: L, val1: L, idx2: L, val2: L) =>
              val vct1 = convert(vectorSet(vct, idx1, val1))
              val vct2 = convert(vectorSet(vct, idx2, val2))
              val vct12 = convert(vectorSet(vct1, idx2, val2))
              val vct21 = convert(vectorSet(vct2, idx1, val1))
              s"vectorSet(vct,$idx1,$val1) = $vct1" |:
                s"vectorSet(vct,$idx2,$val2) = $vct2" |:
                s"vectorSet(vectorSet(vct,$idx1,$val1),$idx2,$val2) = $vct12" |:
                s"vectorSet(vectorSet(vct,$idx2,$val2),$idx1,$val1) = $vct21" |:
                vct12 == vct21
          }
        // return the properties
        p
    }
    checkAll(schemeLaws)

class ConcreteSchemeLatticeTests extends SchemeLatticeTests(ConcreteModularSchemeLattice.SchemeValueLatticeGenerator)
class ConstantSchemeLatticeTests extends SchemeLatticeTests(ConstantModularSchemeLattice.SchemeValueLatticeGenerator)
class TypeSchemeLatticeTests extends SchemeLatticeTests(TypeModularSchemeLattice.SchemeValueLatticeGenerator)
