package maf.language.scheme.lattices

import maf.core._
import maf.language.CScheme.TID
import maf.lattice.interfaces.BoolLattice
import maf.language.scheme._
import maf.language.scheme.primitives.SchemePrimitive
import maf.language.scheme.primitives.SchemeInterpreterBridge

/**
 * Creates the product of two scheme lattices and
 *  becomes a Scheme lattice.
 */
object Product2SchemeLattice {

  import maf.util.MonoidInstances._

  /** The abstract value from the combined lattice. */
  case class Product2[L, R](left: L, right: R)
  object Product2 {
    def injectLeft[L, R](left: L)(implicit rightLattice: Lattice[R]): Product2[L, R] = Product2(left, rightLattice.bottom)
    def injectRight[L, R](right: R)(implicit leftLattice: Lattice[L]): Product2[L, R] = Product2(leftLattice.bottom, right)
  }

  implicit def injectStoreL[L, R, A <: Address](store: Store[A, L])(implicit lattice: Lattice[R]): Store[A, Product2[L, R]] =
    new Store[A, Product2[L, R]] {

      override def canEqual(that: Any): Boolean = store.canEqual(that)

      override def productArity: Int = store.productArity

      override def productElement(n: Int): Any = store.productElement(n)

      def lookup(a: A): Option[Product2[L, R]] =
        store.lookup(a).map(Product2.injectLeft(_))

      def extend(a: A, v: Product2[L, R]): Store[A, Product2[L, R]] =
        injectStoreL[L, R, A](store.extend(a, v.left))
    }

  implicit def injectStoreR[L, R, A <: Address](store: Store[A, R])(implicit lattice: Lattice[L]): Store[A, Product2[L, R]] =
    new Store[A, Product2[L, R]] {
      override def canEqual(that: Any): Boolean = store.canEqual(that)

      override def productArity: Int = store.productArity

      override def productElement(n: Int): Any = store.productElement(n)

      def lookup(a: A): Option[Product2[L, R]] =
        store.lookup(a).map(Product2.injectRight(_))

      def extend(a: A, v: Product2[L, R]): Store[A, Product2[L, R]] =
        injectStoreR[L, R, A](store.extend(a, v.right))
    }

  implicit def projStoreL[L, R, A <: Address](
      store: Store[A, Product2[L, R]]
    )(implicit rightLattice: SchemeLattice[R, A, SchemePrimitive[R, A]]
    ): Store[A, L] =
    new Store[A, L] {
      override def canEqual(that: Any): Boolean = store.canEqual(that)

      override def productArity: Int = store.productArity

      override def productElement(n: Int): Any = store.productElement(n)
      def lookup(a: A): Option[L] = store.lookup(a).map(_.left)
      def extend(a: A, v: L): Store[A, L] = projStoreL(store.extend(a, Product2(v, rightLattice.bottom)))
    }

  implicit def projStoreR[L, R, A <: Address](
      store: Store[A, Product2[L, R]]
    )(implicit leftLattice: SchemeLattice[L, A, SchemePrimitive[L, A]]
    ): Store[A, R] =
    new Store[A, R] {
      override def canEqual(that: Any): Boolean = store.canEqual(that)

      override def productArity: Int = store.productArity

      override def productElement(n: Int): Any = store.productElement(n)
      def lookup(a: A): Option[R] = store.lookup(a).map(_.right)
      def extend(a: A, v: R): Store[A, R] = projStoreR(store.extend(a, Product2(leftLattice.bottom, v)))
    }

  implicit def injBridgeL[L, R, A <: Address](
      bridge: SchemeInterpreterBridge[L, A]
    )(implicit lattice: Lattice[R]
    ): SchemeInterpreterBridge[Product2[L, R], A] = new SchemeInterpreterBridge[Product2[L, R], A] {
    override type Closure = bridge.Closure
    def pointer(exp: SchemeExp): A = bridge.pointer(exp)
    def callcc(
        clo: (SchemeLambdaExp, Environment[A]),
        nam: Option[String],
        pos: Position.Position
      ): Product2[L, R] = Product2.injectLeft(bridge.callcc(clo, nam, pos))
    def currentThread: TID = bridge.currentThread
  }

  implicit def injBridgeR[L, R, A <: Address](
      bridge: SchemeInterpreterBridge[R, A]
    )(implicit lattice: Lattice[L]
    ): SchemeInterpreterBridge[Product2[L, R], A] = new SchemeInterpreterBridge[Product2[L, R], A] {
    override type Closure = bridge.Closure
    def pointer(exp: SchemeExp): A = bridge.pointer(exp)
    def callcc(
        clo: (SchemeLambdaExp, Environment[A]),
        nam: Option[String],
        pos: Position.Position
      ): Product2[L, R] = Product2.injectRight(bridge.callcc(clo, nam, pos))
    def currentThread: TID = bridge.currentThread
  }

  implicit def projBridgeL[L, R, A <: Address](
      bridge: SchemeInterpreterBridge[Product2[L, R], A]
    ): SchemeInterpreterBridge[L, A] = new SchemeInterpreterBridge[L, A] {
    override type Closure = bridge.Closure
    def pointer(exp: SchemeExp): A = bridge.pointer(exp)
    def callcc(
        clo: (SchemeLambdaExp, Environment[A]),
        nam: Option[String],
        pos: Position.Position
      ): L = bridge.callcc(clo, nam, pos).left

    def currentThread: TID = bridge.currentThread
  }

  implicit def projBridgeR[L, R, A <: Address](
      bridge: SchemeInterpreterBridge[Product2[L, R], A]
    ): SchemeInterpreterBridge[R, A] = new SchemeInterpreterBridge[R, A] {
    override type Closure = bridge.Closure
    def pointer(exp: SchemeExp): A = bridge.pointer(exp)
    def callcc(
        clo: (SchemeLambdaExp, Environment[A]),
        nam: Option[String],
        pos: Position.Position
      ): R = bridge.callcc(clo, nam, pos).right

    def currentThread: TID = bridge.currentThread
  }

  /** Projection of SchemePrimitive from Product2[L,R] to L */
  implicit def projL[L, R, A <: Address](
      value: SchemePrimitive[Product2[L, R], A]
    )(implicit rightLattice: SchemeLattice[R, A, SchemePrimitive[R, A]]
    ): SchemePrimitive[L, A] = new SchemePrimitive[L, A] {
    def name: String = value.name

    def call(
        fexp: SchemeExp,
        args: List[(SchemeExp, L)],
        store: Store[A, L],
        scheme: SchemeInterpreterBridge[L, A]
      ): MayFail[(L, Store[A, L]), maf.core.Error] =
      value
        .call(fexp, args.map { case (exp, v) => (exp, Product2.injectLeft(v)) }, injectStoreL(store), scheme)
        .map { case (v, store) =>
          (v.left, projStoreL(store))
        }
  }

  implicit def projR[L, R, A <: Address](
      value: SchemePrimitive[Product2[L, R], A]
    )(implicit leftLattice: SchemeLattice[L, A, SchemePrimitive[L, A]]
    ): SchemePrimitive[R, A] = new SchemePrimitive[R, A] {
    def name: String = value.name

    def call(
        fexp: SchemeExp,
        args: List[(SchemeExp, R)],
        store: Store[A, R],
        scheme: SchemeInterpreterBridge[R, A]
      ): MayFail[(R, Store[A, R]), maf.core.Error] =
      value
        .call(fexp, args.map { case (exp, v) => (exp, Product2.injectRight(v)) }, injectStoreR(store), scheme)
        .map { case (v, store) =>
          (v.right, projStoreR(store))
        }
  }

  implicit def injL[L, R, A <: Address](
      value: SchemePrimitive[L, A]
    )(implicit lattice: SchemeLattice[R, A, SchemePrimitive[R, A]]
    ): SchemePrimitive[Product2[L, R], A] = new SchemePrimitive[Product2[L, R], A] {
    def name: String = value.name

    def call(
        fexp: SchemeExp,
        args: List[(SchemeExp, Product2[L, R])],
        store: Store[A, Product2[L, R]],
        scheme: SchemeInterpreterBridge[Product2[L, R], A]
      ): MayFail[(Product2[L, R], Store[A, Product2[L, R]]), Error] =
      value
        .call(fexp, args.map { case (exp, v) => (exp, v.left) }, store, scheme)
        .map { case (v, store) => (Product2.injectLeft(v), store) }
  }

  implicit def injR[L, R, A <: Address](
      value: SchemePrimitive[R, A]
    )(implicit lattice: SchemeLattice[L, A, SchemePrimitive[L, A]]
    ): SchemePrimitive[Product2[L, R], A] = new SchemePrimitive[Product2[L, R], A] {
    def name: String = value.name

    def call(
        fexp: SchemeExp,
        args: List[(SchemeExp, Product2[L, R])],
        store: Store[A, Product2[L, R]],
        scheme: SchemeInterpreterBridge[Product2[L, R], A]
      ): MayFail[(Product2[L, R], Store[A, Product2[L, R]]), Error] =
      value
        .call(fexp, args.map { case (exp, v) => (exp, v.right) }, store, scheme)
        .map { case (v, store) => (Product2.injectRight(v), store) }
  }

  implicit def product[L, R, A <: Address](
      leftLattice: SchemeLattice[L, A, SchemePrimitive[L, A]],
      rightLattice: SchemeLattice[R, A, SchemePrimitive[R, A]]
    ): SchemeLattice[Product2[L, R], A, SchemePrimitive[Product2[L, R], A]] =
    new SchemeLattice[Product2[L, R], A, SchemePrimitive[Product2[L, R], A]] {

      override def show(v: Product2[L, R]): String = v match {
        case v: Product2[_, _] => s"${v.left} x ${v.right}"
      }

      override def bottom: Product2[L, R] = Product2(leftLattice.bottom, rightLattice.bottom)

      override def top: Product2[L, R] = throw LatticeTopUndefined

      override def join(x: Product2[L, R], y: => Product2[L, R]): Product2[L, R] = (x, y) match {
        case (Product2(left1, right1), Product2(left2, right2)) =>
          Product2(leftLattice.join(left1, left2), rightLattice.join(right1, right2))
      }

      override def subsumes(x: Product2[L, R], y: => Product2[L, R]): Boolean = (x, y) match {
        case (Product2(left1, right1), Product2(left2, right2)) =>
          leftLattice.subsumes(left1, left2) && rightLattice.subsumes(right1, right2)
        case (_, _) => false
      }

      override def eql[B: BoolLattice](x: Product2[L, R], y: Product2[L, R]): B = ???

      override def isTrue(x: Product2[L, R]): Boolean = x match {
        case Product2(left, right) => leftLattice.isTrue(left) || rightLattice.isTrue(right)
        case _                     => false
      }

      override def isFalse(x: Product2[L, R]): Boolean = x match {
        case Product2(left, right) => leftLattice.isFalse(left) || rightLattice.isTrue(right)
        case _                     => false
      }

      override def op(operation: SchemeOp)(args: List[Product2[L, R]]): MayFail[Product2[L, R], maf.core.Error] = {
        val leftArgs = args.map { case Product2(left, _) =>
          left
        }

        val rightArgs = args.map { case Product2(_, right) =>
          right
        }

        // TODO: if both are bottom, return error
        val left = leftLattice.op(operation)(leftArgs).getOrElse(leftLattice.bottom)
        val right = rightLattice.op(operation)(rightArgs).getOrElse(rightLattice.bottom)

        MayFailSuccess(Product2(left, right))
      }

      override def getClosures(x: Product2[L, R]): Set[((SchemeLambdaExp, Environment[A]), Option[String])] = x match {
        case Product2(left, right) => leftLattice.getClosures(left) ++ rightLattice.getClosures(right)
        case _                     => Set()
      }

      override def getPrimitives(x: Product2[L, R]): Set[SchemePrimitive[Product2[L, R], A]] = x match {
        case Product2(left, right) =>
          leftLattice.getPrimitives(left).map(injL(_)(rightLattice)) ++ rightLattice.getPrimitives(right).map(injR(_)(leftLattice))
        case _ => Set()
      }

      override def getContinuations(x: Product2[L, R]): Set[Any] = x match {
        case Product2(left, right) => leftLattice.getContinuations(left) ++ rightLattice.getContinuations(right)
        case _                     => Set()
      }

      override def getPointerAddresses(x: Product2[L, R]): Set[A] = x match {
        case Product2(left, right) => leftLattice.getPointerAddresses(left) ++ rightLattice.getPointerAddresses(right)
        case _                     => Set()
      }

      override def getThreads(x: Product2[L, R]): Set[TID] = x match {
        case Product2(left, right) => leftLattice.getThreads(left) ++ rightLattice.getThreads(right)
        case _                     => Set()
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
        Product2(leftLattice.primitive(projL(x)(rightLattice)), rightLattice.primitive(projR(x)(leftLattice)))

      override def closure(x: (SchemeLambdaExp, Environment[A]), name: Option[String]): Product2[L, R] =
        Product2(leftLattice.closure(x, name), rightLattice.closure(x, name))

      override def symbol(x: String): Product2[L, R] =
        Product2(leftLattice.symbol(x), rightLattice.symbol(x))

      override def cons(car: Product2[L, R], cdr: Product2[L, R]): Product2[L, R] = (car, cdr) match {
        case (Product2(left, right), Product2(left1, right1)) =>
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
