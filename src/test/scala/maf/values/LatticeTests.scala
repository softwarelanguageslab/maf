package maf
package values

import typeclasses.*
import domains.*

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck._
import org.scalatest.propspec._
import org.scalatestplus.scalacheck.Checkers

import cats.syntax.all._
import cats.*

import maf.util.Error
import scala.annotation.tailrec

// makea sure that our equalities make sense
import scala.language.strictEquality

/** TODO[medium] tests for scheme lattice */

abstract class LatticeSpecification extends AnyPropSpec with Checkers:
  // by default, check each property for at least 100 instances
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
  def checkAll(props: Properties): Unit =
    for (name, prop) <- props.properties do
      property(name, test.LatticeTest)(check(prop))
  def newProperties(name: String)(f: Properties => Properties): Properties =
    val p = new Properties(name)
    f(p)
  def conditional(p: Boolean, q: => Boolean): Boolean = !p || q

object LatticeTest:
  type Lat[X] = Either[Error, X]

  given MonadJoin[Lat] with {
    def mjoin[X: Lattice](a: Lat[X], b: Lat[X]): Lat[X] =
      (a, b) mapN ((a, b) => Lattice[X].join(a, b))
  }

  given [A: Lattice, E]: Conversion[Either[E, A], A] =
    _.getOrElse(Lattice[A].bottom)

abstract class LatticeTest[L: Lattice](gen: LatticeGenerator[L])
    extends LatticeSpecification:

  given [A]: CanEqual[A, A] = CanEqual.derived

  val laws: Properties =
    newProperties("Lattice") { p =>
      implicit val arb = gen.anyArb
      implicit val shr = gen.shrink
      val lat: Lattice[L] = implicitly
      import lat._

      /** The subsumption operation is reflexive */
      p.property("∀ a: a ⊑ a") = forAll((a: L) => subsumes(a, a))

      /** Bottom is the lower bound of all elements in the lattice */
      p.property("∀ a: ⊥ ⊑ a") = forAll((a: L) => subsumes(a, bottom))

      /** The join operation is commutative */
      p.property("∀ a, b: a ⊔ b = b ⊔ a") = forAll { (a: L, b: L) =>
        val ab = join(a, b)
        val ba = join(b, a)
        s"a ⊔ b = $ab" |: s"b ⊔ a = $ba" |:
          ab == ba
      }

      /** The join operation is associative */
      p.property("∀ a, b, c: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)") = forAll {
        (a: L, b: L, c: L) =>
          val ab = join(a, b)
          val bc = join(b, c)
          val left = join(ab, c)
          val right = join(a, bc)
          s"a ⊔ b = $ab" |: s"b ⊔ c = $bc" |: s"(a ⊔ b) ⊔ c = $left" |: s"a (b ⊔ c) = $right" |:
            left == right
      }

      /** The join operation is idempotent */
      p.property("∀ a: a ⊔ a = a") = forAll((a: L) => join(a, a) == a)

      /** The join operation is compatible with subsumption */
      p.property("∀ a, b: a ⊑ b ⇒ a ⊔ b = b") = forAll { (b: L) =>
        forAll(gen.le(b)) { (a: L) =>
          val ab = join(a, b)
          s"a ⊔ b = $ab" |:
            subsumes(b, a) ==> (ab == b)
        }
      }
      p
    }
  checkAll(laws)

abstract class BoolLatticeTest[B: BoolLattice](gen: LatticeGenerator[B])
    extends LatticeTest[B](gen):
  val boolLaws: Properties =
    newProperties("Bool") { p =>
      newProperties("BoolLattice") { p =>
        implicit val arb = gen.anyArb
        val lat: BoolLattice[B] = implicitly
        import lat._

        /** Inject preserves truthiness */
        p.property("isTrue(inject(true)) ∧ isFalse(inject(false))") =
          isTrue(inject(true)) && isFalse(inject(false))

        /** Top is both true and false */
        p.property("isTrue(⊤) ∧ isFalse(⊤)") = isTrue(top) && isFalse(top)

        /** Bottom is neither true nor false */
        p.property("¬isTrue(⊥) ∧ ¬isFalse(⊥)") =
          !isTrue(bottom) && !isFalse(bottom)

        /** Not reverses truthiness */
        p.property(
          "∀ a: isTrue(a) ⇒ isFalse(not(a)) ∧ isFalse(a) ⇒ isTrue(not(a))"
        ) = forAll { (a: B) =>
          conditional(isTrue(a), isFalse(not(a))) && conditional(
            isFalse(a),
            isTrue(not(a))
          )
        }

        /** Not is involutive */
        p.property("∀ a: not(not(a)) == a") = forAll((a: B) => not(not(a)) == a)
        p
      }
    }
  checkAll(boolLaws)

abstract class StringLatticeTest[S: StringLattice_[I, C, Sym], C: CharLattice_[
  I,
  Sym,
  S
], Sym: SymbolLattice, I: IntLattice](
    gen: LatticeGenerator[S]
) extends LatticeTest[S](gen):

  import LatticeTest.given
  val stringLaws: Properties =
    newProperties("String") { p =>
      implicit val arb = gen.anyArb
      val lat: StringLattice[S, I, C, Sym] = implicitly
      import lat._

      /** Length preserves bottom */
      p.property("length(⊥) = ⊥") =
        length(bottom).getOrElse(IntLattice[I].bottom) == IntLattice[I].bottom

      /** Length is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ length(a) ⊑ length(b)") = forAll { (b: S) =>
        forAll(gen.le(b)) { (a: S) =>
          conditional(
            subsumes(b, a),
            IntLattice[I].subsumes(length(b), length(a))
          )
        }
      }

      /** Length is sound */
      p.property("∀ a: inject(a.size) ⊑ length(inject(a))") =
        forAll((a: String) =>
          IntLattice[I]
            .subsumes(length(injectString(a)), IntLattice[I].inject(a.size))
        )

      /** Append preservesf bottom */
      p.property("∀ a: append(a, ⊥) = ⊥ = append(⊥, a)") = forAll((a: S) =>
        append(bottom, a).getOrElse(bottom) == bottom && append(a, bottom)
          .getOrElse(bottom) == bottom
      )

      /** Append is monotone */
      p.property(
        "∀ a, b, c: a ⊑ b ⇒ append(a, c) ⊑ append(b, c) ∧ append(c, a) ⊑ append(c, b)"
      ) = forAll { (b: S, c: S) =>
        forAll(gen.le(b)) { (a: S) =>
          conditional(
            subsumes(c, b),
            subsumes(append(a, c), append(a, b)) && subsumes(
              append(c, a),
              append(b, a)
            )
          )
        }
      }

      /** Append is sound */
      p.property("∀ a, b: append(inject(a), inject(b)) ⊑ inject(a ++ b)") =
        forAll((a: String, b: String) =>
          subsumes(
            append(injectString(a), injectString(b)),
            injectString(a ++ b)
          )
        )

      /** Append is associative */
      p.property(
        "∀ a, b, c: append(append(a, b), c) == append(a, append(b, c))"
      ) = forAll((a: S, b: S, c: S) =>
        append(append(a, b).getOrElse(bottom), c)
          .getOrElse(bottom) == append(a, append(b, c)).getOrElse(bottom)
      )

      p
    }
  checkAll(stringLaws)

abstract class IntLatticeTest[
    I: IntLattice,
    B: BoolLattice,
    R: RealLattice,
    Sym: SymbolLattice,
    C: CharLattice_[I, Sym, S],
    S: StringLattice_[I, C, Sym]
](gen: LatticeGenerator[I])
    extends LatticeTest[I](gen):

  import LatticeTest.{Lat, given}
  import cats.instances.*

  val intLaws: Properties =
    newProperties("Int") { p =>
      implicit val arb = gen.anyArb
      val lat: IntLattice[I] = implicitly
      import lat._

      /** Conversion to real preserves bottom */
      p.property("toReal(⊥) = ⊥") = toReal[Lat, R](bottom).getOrElse(
        RealLattice[R].bottom
      ) == RealLattice[R].bottom

      /** Conversion to real is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toReal(a) ⊑ toR(b)") = forAll { (b: I) =>
        forAll(gen.le(b)) { (a: I) =>
          conditional(
            subsumes(b, a),
            RealLattice[R].subsumes(toReal[Lat, R](b), toReal[Lat, R](a))
          )
        }
      }

      /** Conversion to real is sound */
      p.property("∀ a: inject(a.toDouble) ⊑ toReal(inject(a))") =
        forAll((a: Int) =>
          RealLattice[R]
            .subsumes(
              toReal[Lat, R](inject(a)),
              RealLattice[R].inject(a.toDouble)
            )
        )

      /** Random preserves bottom */
      p.property("random(⊥) = ⊥") = random(bottom).getOrElse(bottom) == bottom

      /** Random is sound */
      p.property("∀ a: inject(a.random) ⊑ random(inject(a))") =
        forAll((a: Int) =>
          subsumes(random(inject(a)), inject(MathOps.random(a)))
        )

      /** Random is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ random(a) ⊑ random(b)") = forAll { (b: I) =>
        forAll(gen.le(b)) { (a: I) =>
          conditional(subsumes(b, a), subsumes(random(b), random(a)))
        }
      }

      /** Addition preserves bottom */
      p.property("plus(a, ⊥) = ⊥ = plus(⊥, a)") = forAll((a: I) =>
        plus(a, bottom).getOrElse(bottom) == bottom && plus(bottom, a)
          .getOrElse(bottom) == bottom
      )

      /** Addition is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(subsumes(c, b), subsumes(plus(a, c), plus(a, b)))
          }
      }

      /** Addition is sound */
      p.property("∀ a, b: inject(a + b) ⊑ plus(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          subsumes(plus(inject(a), inject(b)), inject(a + b))
        )

      /** Addition is associative */
      p.property("∀ a, b, c: plus(a, plus(b, c)) == plus(plus(a, b), c)") =
        forAll((a: I, b: I, c: I) =>
          plus(a, plus(b, c).getOrElse(bottom)).getOrElse(bottom) == plus(
            plus(a, b),
            c
          ).getOrElse(bottom)
        )

      /** Addition is commutative */
      p.property("∀ a, b: plus(a, b) == plus(b, a)") = forAll((a: I, b: I) =>
        plus(a, b).getOrElse(bottom) == plus(b, a).getOrElse(bottom)
      )

      /** Subtraction preserves bottom */
      p.property("minus(a, ⊥) = ⊥ = minus(⊥, a)") = forAll((a: I) =>
        minus(a, bottom).getOrElse(bottom) == bottom && minus(bottom, a)
          .getOrElse(bottom) == bottom
      )

      /** Subtraction is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(subsumes(c, b), subsumes(minus(a, c), minus(a, b)))
          }
      }

      /** Subtraction is sound */
      p.property("∀ a, b: inject(a - b) ⊑ minus(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          subsumes(minus(inject(a), inject(b)), inject(a - b))
        )

      /** Subtraction is anticommutative */
      p.property("∀ a, b: minus(a, b) == minus(inject(0), minus(b, a))") =
        forAll((a: I, b: I) =>
          minus(a, b).getOrElse(bottom) == minus(inject(0), minus(b, a))
            .getOrElse(bottom)
        )

      /** Addition preserves bottom */
      p.property("times(a, ⊥) = ⊥ = times(⊥, a)") = forAll((a: I) =>
        times(a, bottom).getOrElse(bottom) == bottom && times(
          bottom,
          a
        ).getOrElse(bottom) == bottom
      )

      /** Addition is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(subsumes(c, b), subsumes(times(a, c), times(a, b)))
          }
      }

      /** Addition is sound */
      p.property("∀ a, b: inject(a + b) ⊑ times(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          subsumes(times(inject(a), inject(b)), inject(a * b))
        )

      /** Addition is associative */
      p.property("∀ a, b, c: times(a, times(b, c)) == times(times(a, b), c)") =
        forAll((a: I, b: I, c: I) =>
          times(a, times(b, c)).getOrElse(bottom) == times(times(a, b), c)
            .getOrElse(bottom)
        )

      /** Addition is commutative */
      p.property("∀ a, b: times(a, b) == times(b, a)") =
        forAll((a: I, b: I) => times(a, b) == times(b, a))

      /** Quotient preserves bottom */
      p.property("div(a, ⊥) = ⊥ = div(⊥, a)") = forAll((a: I) =>
        quotient[Lat](a, bottom).getOrElse(bottom) == bottom && conditional(
          !subsumes(a, inject(0)),
          quotient(bottom, a).getOrElse(bottom) == bottom
        )
      )

      /** Quotient is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(
              subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(
                c,
                inject(0)
              ),
              subsumes(
                quotient(a, c).getOrElse(bottom),
                quotient(a, b).getOrElse(bottom)
              )
            )
          }
      }

      /** Quotient is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ div(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          conditional(
            b != 0,
            subsumes(
              quotient(inject(a), inject(b)).getOrElse(bottom),
              inject(a / b)
            )
          )
        )

      /** Modulo preserves bottom */
      p.property("modulo(a, ⊥) = ⊥ = modulo(⊥, a)") = forAll((a: I) =>
        modulo(a, bottom).getOrElse(bottom) == bottom && conditional(
          !subsumes(a, inject(0)),
          modulo(bottom, a).getOrElse(bottom) == bottom
        )
      )

      /** Modulo is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ modulo(a, b) ⊑ modulo(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(
              subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(
                b,
                inject(0)
              ),
              subsumes(
                modulo(a, c).getOrElse(bottom),
                modulo(a, b).getOrElse(bottom)
              )
            )
          }
      }

      /** Modulo is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ modulo(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          conditional(
            b != 0,
            subsumes(
              modulo(inject(a), inject(b)).getOrElse(bottom),
              inject(MathOps.modulo(a, b))
            )
          )
        )

      /** Remainder preserves bottom */
      p.property("rem(a, ⊥) = ⊥ = rem(⊥, a) (if a ≠ 0)") = forAll((a: I) =>
        remainder(a, bottom).getOrElse(bottom) == bottom && conditional(
          !subsumes(a, inject(0)),
          remainder(bottom, a).getOrElse(bottom) == bottom
        )
      )

      /** Remainder is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ rem(a, b) ⊑ rem(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(
              subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(
                b,
                inject(0)
              ),
              subsumes(
                remainder(a, c).getOrElse(bottom),
                remainder(a, b).getOrElse(bottom)
              )
            )
          }
      }

      /** Remainder is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ rem(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          conditional(
            b != 0,
            subsumes(
              remainder(inject(a), inject(b)).getOrElse(bottom),
              inject(MathOps.remainder(a, b))
            )
          )
        )

      /** Less-than operation preserves bottom */
      p.property("lt(a, ⊥) = ⊥ = lt(⊥, a)") = forAll((a: I) =>
        lt[Lat, B](a, bottom).getOrElse(BoolLattice[B].bottom) == BoolLattice[
          B
        ].bottom && lt[Lat, B](
          bottom,
          a
        ).getOrElse(BoolLattice[B].bottom) == BoolLattice[B].bottom
      )

      /** Less-than operation is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c)") = forAll {
        (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            conditional(
              subsumes(b, c),
              BoolLattice[B].subsumes(lt[Lat, B](a, c), lt[Lat, B](a, b))
            )
          }
      }

      /** Less-than operation is sound */
      p.property("∀ a, b ≠ 0: inject(a < b) ⊑ lt(inject(a), inject(b))") =
        forAll((a: BigInt, b: BigInt) =>
          BoolLattice[B]
            .subsumes(
              lt[Lat, B](inject(a), inject(b)),
              BoolLattice[B].inject(a < b)
            )
        )

      /** To-string operation preserves bottom */
      p.property("toString(⊥) = ⊥") =
        lat.toString(bottom) == StringLattice[S, I, C, Sym].bottom

      /** To-string operation is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toString(a) ⊑ toString(b)") = forAll {
        (b: I) =>
          forAll(gen.le(b)) { (a: I) =>
            conditional(
              subsumes(b, a),
              StringLattice[S, I, C, Sym]
                .subsumes(lat.toString(b), lat.toString(a))
            )
          }
      }

      /** To-string operation is sound */
      p.property("∀ a, b: inject(toString(a)) ⊑ toString(inject(a))") =
        forAll((a: BigInt) =>
          StringLattice[S, I, C, Sym].subsumes(
            lat.toString(inject(a)),
            StringLattice[S, I, C, Sym].injectString(a.toString)
          )
        )

      p
    }
  checkAll(intLaws)

abstract class RealLatticeTest[
    R: RealLattice,
    B: BoolLattice,
    I: IntLattice,
    C: CharLattice_[I, Sym, S],
    Sym: SymbolLattice,
    S: StringLattice_[I, C, Sym]
](gen: LatticeGenerator[R])
    extends LatticeTest[R](gen):
  import cats.instances.*
  import LatticeTest.{Lat, given}
  val realLaws: Properties =
    newProperties("Real") { p =>
      implicit val arb = gen.anyArb
      val lat: RealLattice[R] = implicitly
      import lat._

      /** Integer conversion preserves bottom */
      p.property("toInt(⊥) = ⊥") = toInt[Lat, I](bottom).getOrElse(
        IntLattice[I].bottom
      ) == IntLattice[I].bottom

      /** Integer conversion is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toInt(a) ⊑ toInt(b)") = forAll { (b: R) =>
        forAll(gen.le(b)) { (a: R) =>
          conditional(
            subsumes(b, a),
            IntLattice[I].subsumes(toInt[Lat, I](b), toInt[Lat, I](a))
          )
        }
      }

      /** Integer conversion is sound */
      p.property("∀ a: a.toInt ⊑ toInt(inject(a))") = forAll((a: Double) =>
        IntLattice[I]
          .subsumes(toInt[Lat, I](inject(a)), IntLattice[I].inject(a.toInt))
      )

      /** Ceiling preserves bottom */
      p.property("ceiling(⊥) = ⊥") = ceiling(bottom).getOrElse(bottom) == bottom

      /** Ceiling is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ ceiling(a) ⊑ ceiling(b)") = forAll { (b: R) =>
        forAll(gen.le(b)) { (a: R) =>
          conditional(subsumes(b, a), subsumes(ceiling(b), ceiling(a)))
        }
      }

      /** Ceiling is sound */
      p.property("∀ a: inject(a.ceil) ⊑ ceiling(inject(a))") =
        forAll((a: Double) =>
          subsumes(ceiling(inject(a)), inject(MathOps.ceil(a)))
        )

      /** Log preserves bottom */
      p.property("log(⊥) = ⊥") = log(bottom).getOrElse(bottom) == bottom

      /** Log is monotone */
      /** TODO[easy] failing test p.property("∀ a, b: a ⊑ b ⇒ log(a) ⊑ log(b)")
        * \= forAll { (b: R) => forAll(gen.le(b)) { (a: R) =>
        * conditional(subsumes(b, a), subsumes(log(b), log(a))) } }
        */
      /** Log is sound */
      p.property("∀ a: inject(a.log) ⊑ log(inject(a))") = forAll((a: Double) =>
        conditional(
          a > 0,
          subsumes(log(inject(a)).getOrElse(bottom), inject(scala.math.log(a)))
        )
      )

      /** Random preserves bottom */
      p.property("random(⊥) = ⊥") = random(bottom).getOrElse(bottom) == bottom

      /** Random is sound */
      p.property("∀ a: inject(a.random) ⊑ random(inject(a))") =
        forAll((a: Double) =>
          subsumes(random(inject(a)), inject(MathOps.random(a)))
        )

      /** Random is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ random(a) ⊑ random(b)") = forAll { (b: R) =>
        forAll(gen.le(b)) { (a: R) =>
          conditional(subsumes(b, a), subsumes(random(b), random(a)))
        }
      }

      /** Addition preserves bottom */
      p.property("plus(a, ⊥) = ⊥ = plus(⊥, a)") = forAll((a: R) =>
        plus(a, bottom).getOrElse(bottom) == bottom && plus(bottom, a)
          .getOrElse(bottom) == bottom
      )

      /** Addition is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c)") = forAll {
        (a: R, c: R) =>
          forAll(gen.le(c)) { (b: R) =>
            conditional(subsumes(c, b), subsumes(plus(a, c), plus(a, b)))
          }
      }

      /** Addition is sound */
      p.property("∀ a, b: inject(a + b) ⊑ plus(inject(a), inject(b))") =
        forAll((a: Double, b: Double) =>
          subsumes(plus(inject(a), inject(b)), inject(a + b))
        )
      /* Plus isn't required to be associative or commutative on reals */

      /** Subtraction preserves bottom */
      p.property("minus(a, ⊥) = ⊥ = minus(⊥, a)") = forAll((a: R) =>
        minus(a, bottom).getOrElse(bottom) == bottom && minus(
          bottom,
          a
        ).getOrElse(bottom) == bottom
      )

      /** Subtraction is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c)") = forAll {
        (a: R, c: R) =>
          forAll(gen.le(c)) { (b: R) =>
            conditional(subsumes(c, b), subsumes(minus(a, c), minus(a, b)))
          }
      }

      /** Subtraction is sound */
      p.property("∀ a, b: inject(a - b) ⊑ minus(inject(a), inject(b))") =
        forAll((a: Double, b: Double) =>
          subsumes(minus(inject(a), inject(b)), inject(a - b))
        )
      /* Minus isn't required to be anticommutative on reals */

      /** Multiplication preserves bottom */
      p.property("times(a, ⊥) = ⊥ = times(⊥, a)") = forAll((a: R) =>
        times(a, bottom).getOrElse(bottom) == bottom && times(bottom, a)
          .getOrElse(bottom) == bottom
      )

      /** Multiplication is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c)") = forAll {
        (a: R, c: R) =>
          forAll(gen.le(c)) { (b: R) =>
            conditional(subsumes(c, b), subsumes(times(a, c), times(a, b)))
          }
      }

      /** Multiplication is sound */
      p.property("∀ a, b: inject(a + b) ⊑ times(inject(a), inject(b))") =
        forAll((a: Double, b: Double) =>
          subsumes(times(inject(a), inject(b)), inject(a * b))
        )
      /* Multiplication isn't required to be associative and commutative on reals */

      /** Division preserves bottom */
      p.property("div(a, ⊥) = ⊥ = div(⊥, a)") = forAll((a: R) =>
        div(a, bottom).getOrElse(bottom) == bottom && conditional(
          !subsumes(a, inject(0)),
          div(bottom, a).getOrElse(bottom) == bottom
        )
      )

      /** Division is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c)") = forAll {
        (a: R, c: R) =>
          forAll(gen.le(c)) { (b: R) =>
            conditional(
              subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(
                c,
                inject(0)
              ),
              subsumes(div(a, c).getOrElse(bottom), div(a, b).getOrElse(bottom))
            )
          }
      }

      /** Division is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ div(inject(a), inject(b))") =
        forAll((a: Double, b: Double) =>
          conditional(
            b != 0,
            subsumes(div(inject(a), inject(b)).getOrElse(bottom), inject(a / b))
          )
        )

      /** Less-than operation preserves bottom */
      p.property("lt(a, ⊥) = ⊥ = lt(⊥, a)") = forAll((a: R) =>
        lt[Lat, B](a, bottom)
          .getOrElse(BoolLattice[B].bottom) == BoolLattice[B].bottom && lt[
          Lat,
          B
        ](
          bottom,
          a
        ).getOrElse(BoolLattice[B].bottom) == BoolLattice[B].bottom
      )

      /** Less-than operation is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c)") = forAll {
        (a: R, c: R) =>
          forAll(gen.le(c)) { (b: R) =>
            conditional(
              subsumes(b, c),
              BoolLattice[B].subsumes(lt[Lat, B](a, c), lt[Lat, B](a, b))
            )
          }
      }

      /** Less-than operation is sound */
      p.property("∀ a, b ≠ 0: inject(a < b) ⊑ lt(inject(a), inject(b))") =
        forAll((a: Double, b: Double) =>
          BoolLattice[B]
            .subsumes(
              lt[Lat, B](inject(a), inject(b)),
              BoolLattice[B].inject(a < b)
            )
        )

      /** To-string operation preserves bottom */
      p.property("toString(⊥) = ⊥") =
        lat.toString(bottom) == StringLattice[S, I, C, Sym].bottom

      /** To-string operation is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toString(a) ⊑ toString(b)") = forAll {
        (b: R) =>
          forAll(gen.le(b)) { (a: R) =>
            conditional(
              subsumes(b, a),
              StringLattice[S, I, C, Sym]
                .subsumes(
                  lat.toString[I, C, S, Sym](b),
                  lat.toString[I, C, S, Sym](a)
                )
            )
          }
      }

      /** To-string operation is sound */
      p.property("∀ a: inject(a.toString) ⊑ toString(inject(a))") =
        forAll((a: Double) =>
          StringLattice[S, I, C, Sym].subsumes(
            lat.toString[I, C, S, Sym](inject(a)),
            StringLattice[S, I, C, Sym].injectString(a.toString)
          )
        )
      p
    }
  checkAll(realLaws)

abstract class CharLatticeTest[
    I: IntLattice,
    Sym: SymbolLattice,
    S: StringLattice_[I, C, Sym],
    C: CharLattice_[I, Sym, S]
](gen: LatticeGenerator[C])
    extends LatticeTest[C](gen):
  val charLaws: Properties =
    newProperties("Char")(p => p)
  checkAll(charLaws)

abstract class SymbolLatticeTest[Sym: SymbolLattice](gen: LatticeGenerator[Sym])
    extends LatticeTest[Sym](gen):
  val symbolLaws: Properties =
    newProperties("Symbol")(p => p)
  checkAll(symbolLaws)

//class ConcreteBoolTest
//    extends BoolLatticeTest[Concrete.B](ConcreteBooleanGenerator)
//class ConcreteStringTest
//    extends StringLatticeTest[Concrete.S, Concrete.I](ConcreteStringGenerator)
//class ConcreteIntTest
//    extends IntLatticeTest[Concrete.I, Concrete.B, Concrete.R, Concrete.S](
//      ConcreteIntGenerator
//    )
//class ConcreteRealTest
//    extends RealLatticeTest[Concrete.R, Concrete.B, Concrete.I, Concrete.S](
//      ConcreteRealGenerator
//    )
//class ConcreteCharTest
//    extends CharLatticeTest[Concrete.C](ConcreteCharGenerator)
//class ConcreteSymbolTest
//    extends SymbolLatticeTest[Concrete.Sym](ConcreteSymbolGenerator)

//class TypeBoolTest extends BoolLatticeTest[Type.B](TypeGenerator)
//class TypeStringTest extends StringLatticeTest[Type.S, Type.I](TypeGenerator)
//class TypeIntTest
//    extends IntLatticeTest[Type.I, Type.B, Type.R, Type.S](TypeGenerator)
//class TypeRealTest
//    extends RealLatticeTest[Type.R, Type.B, Type.I, Type.S](TypeGenerator)
//class TypeCharTest extends CharLatticeTest[Type.C](TypeGenerator)
//class TypeSymbolTest extends SymbolLatticeTest[Type.Sym](TypeGenerator)

/* No bool test for constant propagation, as it is equivalent to concrete booleans */
class ConstantPropagationStringTest
    extends StringLatticeTest[
      ConstantPropagation.S,
      ConstantPropagation.C,
      ConstantPropagation.Sym,
      ConstantPropagation.I
    ](
      ConstantPropagationStringGenerator
    )
class ConstantPropagationIntTest
    extends IntLatticeTest[
      ConstantPropagation.I,
      ConstantPropagation.B,
      ConstantPropagation.R,
      ConstantPropagation.Sym,
      ConstantPropagation.C,
      ConstantPropagation.S
    ](ConstantPropagationIntGenerator)
class ConstantPropagationRealTest
    extends RealLatticeTest[
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.I,
      ConstantPropagation.C,
      ConstantPropagation.Sym,
      ConstantPropagation.S
    ](ConstantPropagationRealGenerator)
class ConstantPropagationCharTest
    extends CharLatticeTest[
      ConstantPropagation.I,
      ConstantPropagation.Sym,
      ConstantPropagation.S,
      ConstantPropagation.C
    ](
      ConstantPropagationCharGenerator
    )
class ConstantPropagationSymbolTest
    extends SymbolLatticeTest[ConstantPropagation.Sym](
      ConstantPropagationSymbolGenerator
    )
