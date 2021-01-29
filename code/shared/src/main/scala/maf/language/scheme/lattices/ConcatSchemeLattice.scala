package maf.language.scheme.lattices

import maf.core._
import maf.language.CScheme.TID
import maf.lattice.interfaces.BoolLattice
import maf.language.scheme._
import maf.language.scheme.primitives.SchemePrimitive
import maf.language.scheme.lattices.SchemeOp.IsProcedure

/**
 * Creates the product of two scheme lattices and
 *  becomes a Scheme lattice.
 */
object Product2SchemeLattice {

  import maf.util.MonoidInstances._

  trait StoreWrapper[A <: Address, L] {
    val store: Store[A, L]
  }

  object StoreWrapper {

    /** Unwraps an arbitrary nested store */
    def unwrap(store: Any): Any = store match {
      case v: StoreWrapper[_, _] => unwrap(v.store)
      case _                     => store
    }
  }

  /** The abstract value from the combined lattice. */
  sealed trait Product2[L, R] extends Serializable {
    def left(implicit leftLattice: Lattice[L]): L =
      leftLattice.bottom

    def right(implicit rightLattice: Lattice[R]): R =
      rightLattice.bottom
  }
  case class Product2Elements[L, R](l: L, r: R) extends Product2[L, R] {
    override def left(implicit leftLattice: Lattice[L]): L = l
    override def right(implicit rightLattice: Lattice[R]): R = r
  }
  case class Product2Primitive[L, R, A <: Address](prim: Set[SchemePrimitive[Product2[L, R], A]]) extends Product2[L, R]

  object Product2 {
    def injectLeft[L, R](left: L)(implicit rightLattice: Lattice[R]): Product2[L, R] = Product2(left, rightLattice.bottom)
    def injectRight[L, R](right: R)(implicit leftLattice: Lattice[L]): Product2[L, R] = Product2(leftLattice.bottom, right)
    def apply[L, R](left: L, right: R): Product2[L, R] = Product2Elements(left, right)
  }

  implicit def product[L, R, A <: Address](
      left: SchemeLattice[L, A, SchemePrimitive[L, A]],
      right: SchemeLattice[R, A, SchemePrimitive[R, A]]
    ): SchemeProductLattice[L, R, A] = new SchemeProductLattice[L, R, A] {
    implicit val leftLattice: SchemeLattice[L, A, SchemePrimitive[L, A]] = left
    implicit val rightLattice: SchemeLattice[R, A, SchemePrimitive[R, A]] = right
  }

  trait SchemeProductLattice[L, R, A <: Address] extends SchemeLattice[Product2[L, R], A, SchemePrimitive[Product2[L, R], A]] {
    implicit val leftLattice: SchemeLattice[L, A, SchemePrimitive[L, A]]
    implicit val rightLattice: SchemeLattice[R, A, SchemePrimitive[R, A]]

    private lazy val leftBottom: L = leftLattice.bottom
    private lazy val rightBottom: R = rightLattice.bottom

    override def show(v: Product2[L, R]): String = v match {
      case v: Product2[_, _] => s"${v.left} x ${v.right}"
    }

    override def bottom: Product2[L, R] = Product2(leftLattice.bottom, rightLattice.bottom)

    override def top: Product2[L, R] = throw LatticeTopUndefined

    override def join(x: Product2[L, R], y: => Product2[L, R]): Product2[L, R] = (x, y) match {
      case (Product2Elements(left1, right1), Product2Elements(left2, right2)) =>
        Product2(leftLattice.join(left1, left2), rightLattice.join(right1, right2))
      case (Product2Elements(`leftBottom`, `rightBottom`), Product2Primitive(_)) =>
        y

      case (Product2Primitive(_), Product2Elements(`leftBottom`, `rightBottom`)) =>
        x
      case (Product2Primitive(a), Product2Primitive(b)) =>
        Product2Primitive(a ++ b)

      // anything else cannot be joined
      case (_, _) =>
        println("warning invalid join happening")
        bottom
    }

    override def subsumes(x: Product2[L, R], y: => Product2[L, R]): Boolean = (x, y) match {
      case (Product2Elements(left1, right1), Product2Elements(left2, right2)) =>
        leftLattice.subsumes(left1, left2) && rightLattice.subsumes(right1, right2)
      case (Product2Primitive(prims1), Product2Primitive(prims2)) =>
        prims2.subsetOf(prims1)

      case (_, _) => false
    }

    override def eql[B: BoolLattice](x: Product2[L, R], y: Product2[L, R]): B = ???

    override def isTrue(x: Product2[L, R]): Boolean = x match {
      case Product2Elements(left, right) => leftLattice.isTrue(left) || rightLattice.isTrue(right)
      case _                             => false
    }

    override def isFalse(x: Product2[L, R]): Boolean = x match {
      case Product2Elements(left, right) => leftLattice.isFalse(left) || rightLattice.isFalse(right)
      case _                             => false
    }

    override def op(operation: SchemeOp)(args: List[Product2[L, R]]): MayFail[Product2[L, R], maf.core.Error] = {
      val leftArgs = args.map(_.left)
      val rightArgs = args.map(_.right)

      // TODO: if both are bottom, return error
      val left = leftLattice.op(operation)(leftArgs).getOrElse(leftLattice.bottom)
      val right = rightLattice.op(operation)(rightArgs).getOrElse(rightLattice.bottom)
      val isProcedure = operation match {
        case IsProcedure =>
          args(0) match {
            case Product2Primitive(_) => bool(true)
            case _                    => bottom
          }
        case _ => bottom
      }

      MayFailSuccess(join(isProcedure, Product2(left, right)))
    }

    override def getClosures(x: Product2[L, R]): Set[((SchemeLambdaExp, Environment[A]), Option[String])] = x match {
      case Product2Elements(left, right) => leftLattice.getClosures(left) ++ rightLattice.getClosures(right)
      case _                             => Set()
    }

    override def getPrimitives(x: Product2[L, R]): Set[SchemePrimitive[Product2[L, R], A]] = x match {
      case p: Product2Primitive[L, R, A] => p.prim
      case _                             => Set()
    }

    override def getContinuations(x: Product2[L, R]): Set[Any] = x match {
      case Product2Elements(left, right) => leftLattice.getContinuations(left) ++ rightLattice.getContinuations(right)
      case _                             => Set()
    }

    override def getPointerAddresses(x: Product2[L, R]): Set[A] = x match {
      case Product2Elements(left, right) => leftLattice.getPointerAddresses(left) ++ rightLattice.getPointerAddresses(right)
      case _                             => Set()
    }

    override def getThreads(x: Product2[L, R]): Set[TID] = x match {
      case Product2Elements(left, right) => leftLattice.getThreads(left) ++ rightLattice.getThreads(right)
      case _                             => Set()
    }

    override def number(x: Int): Product2[L, R] =
      Product2(leftLattice.number(x), rightLattice.number(x))

    override def numTop: Product2[L, R] =
      Product2(leftLattice.numTop, rightLattice.numTop)

    override def real(x: Double): Product2[L, R] =
      Product2(leftLattice.real(x), rightLattice.real(x))

    override def string(x: String): Product2[L, R] =
      Product2(leftLattice.string(x), rightLattice.string(x))

    override def bool(x: Boolean): Product2[L, R] =
      Product2(leftLattice.bool(x), rightLattice.bool(x))

    override def char(x: Char): Product2[L, R] =
      Product2(leftLattice.char(x), rightLattice.char(x))

    override def charTop: Product2[L, R] =
      Product2(leftLattice.charTop, rightLattice.charTop)

    override def primitive(x: SchemePrimitive[Product2[L, R], A]): Product2[L, R] =
      Product2Primitive(Set((x)))

    override def closure(x: (SchemeLambdaExp, Environment[A]), name: Option[String]): Product2[L, R] =
      Product2(leftLattice.closure(x, name), rightLattice.closure(x, name))

    override def symbol(x: String): Product2[L, R] =
      Product2(leftLattice.symbol(x), rightLattice.symbol(x))

    override def cons(car: Product2[L, R], cdr: Product2[L, R]): Product2[L, R] = (car, cdr) match {
      case (Product2Elements(left, right), Product2Elements(left1, right1)) =>
        Product2(leftLattice.cons(left, left1), rightLattice.cons(right, right1))
    }

    override def nil: Product2[L, R] =
      Product2(leftLattice.nil, rightLattice.nil)

    override def pointer(a: A): Product2[L, R] =
      Product2(leftLattice.pointer(a), rightLattice.pointer(a))

    override def cont(k: Any): Product2[L, R] =
      Product2(leftLattice.cont(k), rightLattice.cont(k))

    override def thread(tid: TID): Product2[L, R] =
      Product2(leftLattice.thread(tid), rightLattice.thread(tid))

    override def lock(threads: Set[TID]): Product2[L, R] =
      Product2(leftLattice.lock(threads), rightLattice.lock(threads))

    override def acquire(lock: Product2[L, R], caller: TID): MayFail[Product2[L, R], maf.core.Error] =
      ???

    override def release(lock: Product2[L, R], caller: TID): MayFail[Product2[L, R], maf.core.Error] = ???

    override def void: Product2[L, R] =
      Product2(leftLattice.void, rightLattice.void)

  }
}

/**
 * This trait provides an empty abstract Scheme domain, it is useful for extending an existing
 * abstract domain (with additional values or operations) by using for example the Product2
 * combiner.
 */
trait EmptyDomainSchemeLattice[L, A <: Address, P <: Primitive] extends SchemeLattice[L, A, P] {

  /** A lattice that provides the basic lattice operations */
  val lattice: Lattice[L]

  def show(v: L): String = lattice.show(v)
  def bottom: L = lattice.bottom
  def top: L = lattice.top
  def join(x: L, y: => L): L = lattice.join(x, y)
  def subsumes(x: L, y: => L): Boolean = lattice.subsumes(x, y)
  def eql[B: BoolLattice](x: L, y: L): B = lattice.eql(x, y)

  def isTrue(x: L): Boolean = false
  def isFalse(x: L): Boolean = false
  def op(op: SchemeOp)(args: List[L]): MayFail[L, maf.core.Error] = MayFail.failure(OperatorNotApplicable(op.name, args))

  def getClosures(x: L): Set[((SchemeLambdaExp, Environment[A]), Option[String])] = Set()

  def getPrimitives(x: L): Set[P] = Set()

  def getContinuations(x: L): Set[Any] = Set()

  def getPointerAddresses(x: L): Set[A] = Set()

  def getThreads(x: L): Set[TID] = Set()

  def number(x: Int): L = bottom

  def numTop: L = bottom

  def real(x: Double): L = bottom

  def string(x: String): L = bottom

  def bool(x: Boolean): L = bottom

  def char(x: Char): L = bottom

  def charTop: L = bottom

  def primitive(x: P): L = bottom

  def closure(x: (SchemeLambdaExp, Environment[A]), name: Option[String]): L = bottom

  def symbol(x: String): L = bottom

  def cons(car: L, cdr: L): L = bottom

  def nil: L = bottom

  def pointer(a: A): L = bottom

  def cont(k: Any): L = bottom

  def thread(tid: TID): L = bottom

  def lock(threads: Set[TID]): L = bottom

  def acquire(lock: L, caller: TID): MayFail[L, maf.core.Error] = MayFail.failure(TypeError("cannot lock on value", lock))

  def release(lock: L, caller: TID): MayFail[L, maf.core.Error] = MayFail.failure(TypeError("cannot release on value", lock))

  def void: L = bottom

}
