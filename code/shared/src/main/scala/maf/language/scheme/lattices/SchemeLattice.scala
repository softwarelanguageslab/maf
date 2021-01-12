package maf.language.scheme.lattices

import maf.core._
import maf.language.CScheme.TID
import maf.language.scheme._

/** A lattice for Scheme should support the following operations */
trait SchemeLattice[L, A <: Address, P <: Primitive] extends Lattice[L] {

  // TODO: make this a type parameter for type safety!
  type K = Any

  /** Can this value be considered true for conditionals? */
  def isTrue(x: L): Boolean

  /** Can this value be considered false for conditionals? */
  def isFalse(x: L): Boolean

  /** Performs an SchemeOp on the abstract values */
  def op(op: SchemeOp)(args: List[L]): MayFail[L, Error]

  /** Conjunction */
  def and(x: L, y: => L): L = (isTrue(x), isFalse(x)) match {
    case (true, false)  => y /* x is true: return y */
    case (false, true)  => bool(false) /* x is false: return false */
    case (false, false) => bottom /* x is not true nor false, it is therefore bottom */
    case (true, true)   => join(y, bool(false)) /* either x is true, and we have y, or x is false, and we have false */
  }

  /** Disjunction */
  def or(x: L, y: => L): L = (isTrue(x), isFalse(x)) match {
    case (true, false)  => x /* x is true, return x */
    case (false, true)  => y /* x is false, return y */
    case (false, false) => bottom /* x is not true nor false, it is therefore bottom */
    case (true, true)   => join(x, y) /* either x is true, and we have x, or x is false, and we have y */
  }

  /** The representation of a closure */
  type Env = Environment[A]
  type Closure = (SchemeLambdaExp, Env)

  /** Extract closures contained in this value */
  def getClosures(x: L): Set[(Closure, Option[String])]

  /** Extract primitives contained in this value */
  def getPrimitives(x: L): Set[P]

  /** Extract continuations contained in this value */
  def getContinuations(x: L): Set[K]

  /** Extract pointers contained in this value */
  def getPointerAddresses(x: L): Set[A]

  /** Extract the thread identifiers in this value */
  def getThreads(x: L): Set[TID]

  /** Injection of an integer */
  def number(x: Int): L

  /** Top element for all integers */
  def numTop: L

  /** Injection of a float */
  def real(x: Double): L

  /** Injection of a string */
  def string(x: String): L

  /** Injection of a boolean */
  def bool(x: Boolean): L

  /** Injection of a character */
  def char(x: Char): L

  /** Injection of a primitive function */
  def primitive(x: P): L

  /** Injection of a closure */
  def closure(x: Closure, name: Option[String]): L

  /** Injection of a symbol */
  def symbol(x: String): L

  /** Injection of a cons cell. */
  def cons(car: L, cdr: L): L

  /**
   * Extract the car of a cons-cell
   *    TODO: any function that has a signature like L* -> MayFail[L, Error] should become a SchemeOp
   */
  def car(x: L): MayFail[L, Error]

  /** Extract the cdr of a cons-cell */
  def cdr(x: L): MayFail[L, Error]

  /** Injection of the nil value */
  def nil: L

  /** Injection of a pointer (to a cons cell, vector, etc.) */
  def pointer(a: A): L

  /** Injection of a continuation */
  def cont(k: K): L

  /** Constructs a new vector */
  def vector(size: L, init: L): MayFail[L, Error]

  /** Accesses an element of a vector */
  def vectorRef(vector: L, index: L): MayFail[L, Error]

  /** Changes an element of a vector */
  def vectorSet(
      vector: L,
      index: L,
      newval: L
    ): MayFail[L, Error]

  /** Injection of a thread identifier */
  def thread(tid: TID): L

  /** Creates a new lock. */
  def lock(threads: Set[TID]): L

  /** Acquires a given lock. */
  def acquire(lock: L, caller: TID): MayFail[L, Error]

  /** Releases a given lock. */
  def release(lock: L, caller: TID): MayFail[L, Error]

  def void: L

  object Injector {
    implicit def inject(c: Closure, name: Option[String]): L = closure(c, name)
    implicit def inject(car: L, cdr: L): L = cons(car, cdr)
    implicit def inject(a: Any): L = a match {
      case i: Int     => number(i)
      case r: Double  => real(r)
      case s: String  => string(s)
      case b: Boolean => bool(b)
      case c: Char    => char(c)
      //case p: P       => primitive(p)
      case s: Symbol => symbol(s.name)
      //case a: A       => pointer(a)
      case Nil => nil
      case v   => throw new Exception(s"Attempting to inject unknown value $v.")
    }
  }
}

object SchemeLattice {
  def apply[L, A <: Address, P <: Primitive](
      implicit lat: SchemeLattice[L, A, P]
    ): SchemeLattice[L, A, P] = lat
}
