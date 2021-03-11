package maf.language.contracts.lattices
import maf.language.contracts.ScLattice

import maf.lattice.interfaces.{BoolLattice}
import maf.util.SmartHash
import maf.util.Ordable
import maf.util.PartialLattice
import maf.core.Primitive
import maf.core.MayFail
import maf.core.OperatorNotApplicable
import maf.language.contracts.ScSchemeLattice
import maf.core.Address

trait ScAbstractValues[A <: Address] {
  import ScLattice._
  import ScOp._

  sealed trait ValueExt extends SmartHash with Ordable

  case class Opqs(opqs: Set[Opq]) extends ValueExt {
    def ord = 1
  }

  case class Arrs(arrs: Set[Arr[A]]) extends ValueExt {
    def ord = 2
  }

  case class Grds(grds: Set[Grd[A]]) extends ValueExt {
    def ord = 3
  }

  case class Blames(blames: Set[Blame]) extends ValueExt {
    def ord = 4
  }

  case class Thunks(thunks: Set[Thunk[A]]) extends ValueExt {
    def ord = 5
  }

  case class Flats(flats: Set[Flat[A]]) extends ValueExt {
    def ord = 6
  }

  case class Clos(clos: Set[Clo[A]]) extends ValueExt {
    def ord = 7
  }

  object Values {
    implicit val partialLattice: PartialLattice[ValueExt] = new PartialLattice[ValueExt] {
      override def join(x: ValueExt, y: => ValueExt): ValueExt = (x, y) match {
        case (Opqs(a), Opqs(b))     => Opqs(a ++ b)
        case (Arrs(a), Arrs(b))     => Arrs(a ++ b)
        case (Grds(a), Grds(b))     => Grds(a ++ b)
        case (Blames(a), Blames(b)) => Blames(a ++ b)
        case (Thunks(a), Thunks(b)) => Thunks(a ++ b)
        case (Flats(a), Flats(b))   => Flats(a ++ b)
        case (Clos(a), Clos(b))     => Clos(a ++ b)
        case _                      => throw new Exception(s"Illegal join $x $y")
      }

      override def subsumes(x: ValueExt, y: => ValueExt): Boolean =
        if (x == y) {
          true
        } else {
          (x, y) match {
            case (Opqs(a), Opqs(b))     => b.subsetOf(a)
            case (Arrs(a), Arrs(b))     => b.subsetOf(a)
            case (Grds(a), Grds(b))     => b.subsetOf(a)
            case (Blames(a), Blames(b)) => b.subsetOf(a)
            case (Thunks(a), Thunks(b)) => b.subsetOf(a)
            case (Clos(a), Clos(b))     => b.subsetOf(a)
            case _                      => false
          }
        }

      // TODO[high]
      override def eql[B: BoolLattice](x: ValueExt, y: ValueExt): B = ???
    }

    def isRefinedOpq(value: ValueExt, refinements: Set[String]): Boolean = value match {
      case Opqs(opq) => opq.forall(o => o.refinementSet.intersect(refinements).nonEmpty)
      case _         => false
    }

    def isArithmeticOperand(value: ValueExt): Boolean =
      isRefinedOpq(value, Set("integer?", "real?"))

    def isOpqInteger(value: ValueExt): Boolean =
      isRefinedOpq(value, Set("integer?"))

    def isOpqReal(value: ValueExt): Boolean =
      isRefinedOpq(value, Set("real?"))

    def isPred[L, P <: Primitive](value: ValueExt, refinements: Set[String])(implicit lat: ScSchemeLattice[L, A, P]): L =
      lat.schemeLattice.bool(isRefinedOpq(value, refinements))

    def numOp[L, P <: Primitive](args: List[ValueExt])(implicit schemeLattice: ScSchemeLattice[L, A, P]): MayFail[L, maf.core.Error] =
      if (args.forall(isRefinedOpq(_, Set("integer?", "real?"))))
        MayFail.success(schemeLattice.opq(Opq(Set("integer?", "real?"))))
      else MayFail.failure(OperatorNotApplicable("+", args))

    /** Defines some operations between values of the Sc abstract domain */
    def op[L, P <: Primitive](op: ScOp)(args: List[ValueExt])(implicit lat: ScSchemeLattice[L, A, P]): MayFail[L, maf.core.Error] = {
      import maf.language.scheme.lattices.SchemeOp._
      op.checkArity(args)
      op match {
        case IsDependentContract =>
          MayFail.success(args(0) match {
            case _: Grds => lat.schemeLattice.bool(true)
            case _       => lat.schemeLattice.bool(false)
          })

        case IsMonitored =>
          MayFail.success(args(0) match {
            case _: Grds => lat.schemeLattice.bool(true)
            case _       => lat.schemeLattice.bool(false)
          })

        case IsPair =>
          MayFail.success(isPred(args(0), Set("pair?", "cons?")))

        case IsNonZero =>
          MayFail.success(isPred(args(0), Set("nonzero?")))

        case ScSchemeOp(op) =>
          op match {
            case Plus | Minus | Times | Quotient | Div => numOp(args)

            case IsProcedure =>
              args(0) match {
                case Arrs(_) | Flats(_) | Clos(_) =>
                  MayFail.success(lat.schemeLattice.bool(true))
                case _ =>
                  MayFail.success(isPred(args(0), Set(op.name)))
              }

            case IsNull | IsBoolean | IsCons | IsPointer | IsChar | IsSymbol | IsString | IsInteger | IsReal | IsVector | IsThread | IsLock |
                IsInputPort | IsOutputPort =>
              MayFail.success(isPred(args(0), Set(op.name)))

            // if all operands are opaque values then we return an opaque value without any refinements,
            // as the operation might have violated some of the contracts in the refinements
            case _ if args.forall {
                  case Opqs(_) => true
                  case _       => false
                } =>
              MayFail.success(lat.opq(Opq(Set())))

            case _ => MayFail.failure(OperatorNotApplicable(op.name, args))
          }
        case _ => MayFail.failure(OperatorNotApplicable(op.name, args))
      }
    }

  }
}
