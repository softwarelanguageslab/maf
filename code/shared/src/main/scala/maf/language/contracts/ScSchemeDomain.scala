package maf.language.contracts

import maf.util.ProductLattice
import maf.language.scheme.lattices._
import maf.language.scheme.primitives._
import maf.core.MayFail
import maf.core.Lattice
import maf.language.contracts.lattices.ScAbstractValues
import maf.language.contracts.primitives.ScLatticePrimitives
import maf.language.contracts.lattices.ScOp.ScSchemeOp
import maf.language.scheme.lattices.SchemeOp.IsInteger
import maf.language.scheme.lattices.SchemeOp.IsReal
import maf.language.scheme.lattices.SchemeOp.IsBoolean
import maf.core.Address
import maf.lattice.interfaces.BoolLattice

trait ScSchemeDomain[A <: Address] extends ScAbstractValues[A] { outer =>
  import maf.language.scheme.lattices.Product2SchemeLattice._
  import ScLattice._
  import maf.util.ProductLattice._
  import maf.util.MonoidInstances._
  import maf.language.contracts.lattices.ScOp

  type S
  type B
  type I
  type R
  type C
  type Sym

  type P = SchemePrimitive[ProductLattice[ValueExt], A]
  type Q = SchemePrimitive[Product2[L, modularLattice.Elements], A]
  type L = ProductLattice[ValueExt]
  type V = Product2[L, modularLattice.Elements]
  type Value = V
  type Prim = SchemePrimitive[V, A]

  implicit val boolLattice: BoolLattice[B]

  /** Acccess to Scheme primitives */
  val schemePrimitives: SchemePrimitives[V, A]

  /** The collection of Sc language specific primitives */
  lazy val scPrimitives = new ScLatticePrimitives[V, A]()(lattice)

  /*
   * A map from names of primitives to the primitives themselves
   */
  val primMap: Map[String, SchemePrimitive[V, A]] =
    (schemePrimitives.allPrimitives.map(p => (p.name, p)) ++ scPrimitives.allPrimitives.map(p => (p.name, p))).toMap

  val modularLattice: ModularSchemeLattice[A, S, B, I, R, C, Sym]

  implicit lazy val valueExtProductLattice: Lattice[L] =
    productLattice(Values.partialLattice)

  implicit lazy val leftLattice: SchemeLattice[L, A, P] = new EmptyDomainSchemeLattice[L, A, P] {
    val lattice = valueExtProductLattice
  }

  implicit lazy val rightLattice = modularLattice.schemeLattice

  private def mayFailJoin(x: MayFail[V, maf.core.Error], y: => MayFail[V, maf.core.Error]): MayFail[V, maf.core.Error] =
    mayFail(latticeMonoid(lattice.schemeLattice)).append(x, y)

  private def mayFailJoin(xs: Seq[MayFail[V, maf.core.Error]]): MayFail[V, maf.core.Error] =
    xs.reduce((a, b) => mayFailJoin(a, b))

  implicit lazy val schemeLattice: SchemeLattice[V, A, Q] =
    new SchemeProductLattice[L, modularLattice.Elements, A] {
      private val rightBottom = outer.rightLattice.bottom

      /**
       * Equate a OPQ{real? / integer?} to numTop, only when the value (from the right lattice) is not precise enough
       * to carry information about the fact that it is a number.
       */
      private def arithOp(operation: SchemeOp)(left: V, right: V): MayFail[V, maf.core.Error] = (left.right, right.right) match {
        case (`rightBottom`, _) if left.left.contains(Values.isArithmeticOperand) =>
          op(operation)(List(Product2.injectRight(rightLattice.numTop), right))
        case (_, `rightBottom`) if left.left.contains(Values.isArithmeticOperand) =>
          op(operation)(List(left, Product2.injectRight(rightLattice.numTop)))
        case _ =>
          super.op(operation)(List(left, right))
      }

      implicit override val leftLattice: SchemeLattice[L, A, SchemePrimitive[L, A]] =
        outer.leftLattice

      implicit override val rightLattice: SchemeLattice[modularLattice.Elements, A, SchemePrimitive[modularLattice.Elements, A]] =
        outer.rightLattice

      override def op(
          operation: SchemeOp
        )(
          args: List[V]
        ): MayFail[V, maf.core.Error] = {
        import SchemeOp._
        operation.checkArity(args)
        operation match {
          case Plus | Minus | Times | Quotient | Div =>
            arithOp(operation)(args(0), args(1))

          // Computations on leftLattice values that return values from the Product2lattice
          case _ =>
            val op = ScOp.ScSchemeOp(operation)

            val result = ProductLattice
              .op(args.map(_.left), (args: List[ValueExt]) => Values.op(op)(args)(lattice))(
                latticeMonoid(lattice.schemeLattice)
              )

            mayFailJoin(result, super.op(operation)(args))
        }
      }
    }

  lazy val lattice: ScSchemeLattice[V, A, Q] = new ScSchemeLattice[V, A, Q] {
    val schemeLattice: SchemeLattice[V, A, Q] = outer.schemeLattice

    /*==================================================================================================================*/

    def grd(grd: Grd[A]): V =
      Product2.injectLeft(ProductLattice(Grds(Set(grd))))

    def arr(arr: Arr[A]): V =
      Product2.injectLeft(ProductLattice(Arrs(Set(arr))))

    def blame(blame: Blame): V =
      Product2.injectLeft(ProductLattice(Blames(Set(blame))))

    def thunk(thunk: Thunk[A]): V =
      Product2.injectLeft(ProductLattice(Thunks(Set(thunk))))

    def opq(opq: Opq): V =
      Product2.injectLeft(ProductLattice(Opqs(Set(opq))))

    def flat(flat: Flat[A]): V =
      Product2.injectLeft(ProductLattice(Flats(Set(flat))))

    def closure(clo: Clo[A]): V =
      Product2.injectLeft(ProductLattice(Clos(Set(clo))))

    /*==================================================================================================================*/

    /** Extract a set of arrow (monitors on functions) from the abstract value */
    def getArr(value: V): Set[Arr[A]] =
      value.left.vs
        .flatMap {
          case v @ Arrs(_) => Some(v)
          case _           => None
        }
        .flatMap(_.arrs)
        .toSet

    /** Extract a set of blames from the abstract value */
    def getBlames(value: V): Set[Blame] =
      value.left.vs
        .flatMap {
          case v @ Blames(_) => Some(v)
          case _             => None
        }
        .flatMap(_.blames)
        .toSet

    /** Extract a set of guards from the abstract value */
    def getGrd(value: V): Set[Grd[A]] =
      value.left.vs
        .flatMap {
          case v @ Grds(_) => Some(v)
          case _           => None
        }
        .flatMap(_.grds)
        .toSet

    /** Extract a set of opaque values from the abstract value */
    def getOpq(value: V): Set[Opq] =
      value.left.vs
        .flatMap {
          case v @ Opqs(_) => Some(v)
          case _           => None
        }
        .flatMap(_.opqs)
        .toSet

    /** Extracts the set of thunks from the abstract domain */
    def getThunk(value: V): Set[Thunk[A]] =
      value.left.vs
        .flatMap {
          case v @ Thunks(_) => Some(v)
          case _             => None
        }
        .flatMap(_.thunks)
        .toSet

    def getFlat(value: V): Set[Flat[A]] =
      value.left.vs
        .flatMap {
          case v @ Flats(_) => Some(v)
          case _            => None
        }
        .flatMap(_.flats)
        .toSet

    def getClosure(value: V): Set[Clo[A]] =
      value.left.vs
        .flatMap {
          case v @ Clos(_) => Some(v)
          case _           => None
        }
        .flatMap(_.clos)
        .toSet

    /*==================================================================================================================*/

    def isDefinitelyOpq(value: V): Boolean =
      value.left.vs.size != 0 &&
        value.left.vs.size == getOpq(value).size

    def isDefinitelyArrow(value: V): Boolean =
      value.left.vs.size != 0 &&
        value.left.vs.size == getArr(value).size

    /** Returns true if the value is possible a blame */
    def isBlame(value: V): Boolean =
      value.left.vs.filter {
        case _: Blames => true
        case _         => false
      }.size >= 1

    /** Returns true if the value is possibly a thunk */
    def isThunk(value: V): Boolean =
      value.left.vs.filter {
        case _: Thunks => true
        case _         => false
      }.size >= 1

    def isFlat(value: V): Boolean =
      value.left.vs.filter {
        case _: Flats => true
        case _        => false
      }.size >= 1

    def isClosure(value: V): Boolean =
      value.left.vs.filter {
        case _: Clos => true
        case _       => false
      }.size >= 1

    /*==================================================================================================================*/
    private def isRefinedTo(value: V, set: Set[String]): V =
      if (getOpq(value).flatMap(_.refinementSet).intersect(set).size >= 1) {
        schemeLattice.bool(true)
      } else if (getOpq(value).size >= 1) {
        Product2.injectRight(modularLattice.Element(modularLattice.Bool(boolLattice.top)))
      } else bottom

    private def specialOp(op: ScOp)(args: List[V]): MayFail[V, maf.core.Error] = {
      op.checkArity(args)
      op match {
        case ScOp.IsTrue =>
          mayFailJoin(
            List(
              MayFail.success(schemeLattice.bool(schemeLattice.isTrue(args(0)))),
              MayFail.success(isRefinedTo(args(0), Set("true?")))
            )
          )

        case ScOp.IsFalse =>
          MayFail.success(schemeLattice.bool(schemeLattice.isFalse(args(0))))

        case ScOp.IsNumber =>
          mayFailJoin(
            List(
              MayFail.success(
                schemeLattice.bool(
                  List(schemeLattice.op(IsInteger)(args), schemeLattice.op(IsReal)(args))
                    .foldLeft(false)((a, b) => b.map(v => a || schemeLattice.isTrue(v)).getOrElse(false))
                )
              ),
              MayFail.success(isRefinedTo(args(0), Set("integer?", "real?")))
            )
          )

        case ScOp.IsAny =>
          MayFail.success(schemeLattice.bool(true))

        case ScOp.IsBool =>
          schemeLattice.op(IsBoolean)(args)

        case _ => MayFail.success(bottom)
      }
    }

    def op(op: ScOp)(args: List[V]): MayFail[V, maf.core.Error] =
      mayFailJoin(
        List(
          ProductLattice
            .op(args.map(_.left), (args: List[ValueExt]) => Values.op(op)(args)(lattice))(
              latticeMonoid(lattice.schemeLattice)
            ),
          op match {
            case ScSchemeOp(op) => schemeLattice.op(op)(args)
            case _              => MayFail.success(bottom)
          },
          specialOp(op)(args)
        )
      )

  }
}
