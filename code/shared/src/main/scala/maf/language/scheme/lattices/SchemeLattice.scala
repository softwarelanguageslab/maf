package maf.language.scheme.lattices

import maf.core._
import maf.lattice.interfaces._
import maf.language.CScheme.TID
import maf.language.ContractScheme.ContractValues._
import maf.language.scheme._

/** A lattice for Scheme should support the following operations */
trait SchemeLattice[L, A <: Address] extends Lattice[L] with LatticeWithAddrs[L, Address]:

    // TODO: make this a type parameter for type safety!
    type K = Any

    /** Can this value be considered true for conditionals? */
    def isTrue(x: L): Boolean

    /** Can this value be considered false for conditionals? */
    def isFalse(x: L): Boolean

    /* Can this value be considered an opaque value */
    def isOpq(x: L): Boolean

    /** Performs an SchemeOp on the abstract values */
    def op(op: SchemeOp)(args: List[L]): MayFail[L, Error]

    /** Conjunction */
    def and(x: L, y: => L): L = (isTrue(x), isFalse(x)) match
        case (true, false)  => y /* x is true: return y */
        case (false, true)  => bool(false) /* x is false: return false */
        case (false, false) => bottom /* x is not true nor false, it is therefore bottom */
        case (true, true)   => join(y, bool(false)) /* either x is true, and we have y, or x is false, and we have false */

    /** Disjunction */
    def or(x: L, y: => L): L = (isTrue(x), isFalse(x)) match
        case (true, false)  => x /* x is true, return x */
        case (false, true)  => y /* x is false, return y */
        case (false, false) => bottom /* x is not true nor false, it is therefore bottom */
        case (true, true)   => join(x, y) /* either x is true, and we have x, or x is false, and we have y */

    /** The representation of a closure */
    type Env = Environment[A]
    type Closure = (SchemeLambdaExp, Env)

    /** Extract closures contained in this value */
    def getClosures(x: L): Set[Closure]

    /** Extract primitives contained in this value */
    def getPrimitives(x: L): Set[String]

    /** Extract continuations contained in this value */
    def getContinuations(x: L): Set[K]

    /** Extract pointers contained in this value */
    def getPointerAddresses(x: L): Set[A]

    /** Extract the thread identifiers in this value */
    def getThreads(x: L): Set[TID]

    /** Extract the blames in this value */
    def getBlames(x: L): Set[Blame]

    /** Extract the guard values in this value */
    def getGrds(x: L): Set[Grd[L]]

    /** Extract the arrow values in this value */
    def getArrs(x: L): Set[Arr[L]]

    /** Extract the flat contract vlaues in this value */
    def getFlats(x: L): Set[Flat[L]]

    /** Extract the struct values from this abstract value */
    def getStructs(x: L): Set[Struct[L]]

    /** Extract the constructor from the abstract domain */
    def getStructConstructor(x: L): Set[StructConstructor]

    /** Extract the struct predicates from the abstract domain */
    def getStructPredicates(x: L): Set[StructPredicate]

    /** Extract the getters/setter values from this abstract value */
    def getGetterSetter(x: L): Set[StructSetterGetter]

    /** Injection of an integer */
    def number(x: BigInt): L

    /** Top element for all integers */
    def numTop: L

    /** Injection of a float */
    def real(x: Double): L

    /** Top element for floats */
    def realTop: L

    /** Injection of a string */
    def string(x: String): L

    /** Top element for strings */
    def stringTop: L

    /** Injection of a boolean */
    def bool(x: Boolean): L

    /** Top element for boolean */
    def boolTop: L = join(bool(true), bool(false))

    /** Injection of a character */
    def char(x: Char): L

    /** Top element for all characters */
    def charTop: L

    /** Injection of a primitive function */
    def primitive(x: String): L

    /** Injection of a closure */
    def closure(x: Closure): L

    /** Injection of a symbol */
    def symbol(x: String): L

    /** Top element for all symbols */
    def symbolTop: L

    /** Injection of a cons cell. */
    def cons(car: L, cdr: L): L

    /** Extract the car of a cons-cell */
    def car(x: L): MayFail[L, Error] = op(SchemeOp.Car)(List(x))

    /** Extract the cdr of a cons-cell */
    def cdr(x: L): MayFail[L, Error] = op(SchemeOp.Cdr)(List(x))

    /** Injection of the nil value */
    def nil: L

    /** Injection of a pointer (to a cons cell, vector, etc.) */
    def pointer(a: A): L

    /** Injection of a continuation */
    def cont(k: K): L

    /** Constructs a new vector */
    def vector(size: L, init: L): MayFail[L, Error] = op(SchemeOp.MakeVector)(List(size, init))

    /** Accesses an element of a vector */
    def vectorRef(vector: L, index: L): MayFail[L, Error] = op(SchemeOp.VectorRef)(List(vector, index))

    /** Changes an element of a vector */
    def vectorSet(
        vector: L,
        index: L,
        newval: L
      ): MayFail[L, Error] = op(SchemeOp.VectorSet)(List(vector, index, newval))

    /** Injection of a thread identifier */
    def thread(tid: TID): L

    /** Creates a new lock. */
    def lock(threads: Set[TID]): L

    /** Acquires a given lock. */
    def acquire(lock: L, caller: TID): MayFail[L, Error]

    /** Releases a given lock. */
    def release(lock: L, caller: TID): MayFail[L, Error]

    /** Injection of a blame in the abstract domain */
    def blame(blame: Blame): L

    /** Injection of a guard value in the abstract domain */
    def grd(grd: Grd[L]): L

    /** Injection of an arrow value in the abstract domain */
    def arr(arr: Arr[L]): L

    /** Injection of a flat value in the abstract domain */
    def flat(flt: Flat[L]): L

    /** Injection of an opaque value in the abstract domain */
    def opq(o: Opq): L

    /** Injection of a struct in the abstract domain */
    def struct(struct: Struct[L]): L

    /** Injection of a struct field setter/getter in the abstract domain */
    def structSetterGetter(setterGetter: StructSetterGetter): L

    /** Injection of a struct constructor in the abstract domain */
    def structConstructor(constr: StructConstructor): L

    /** Injection of struct predicate in the abstract domain */
    def structPredicate(pred: StructPredicate): L

    def void: L

    def eq(x: L, y: L)(comparePtr: MaybeEq[A]): L

    object Injector:
        implicit def inject(c: Closure): L = closure(c)
        implicit def inject(car: L, cdr: L): L = cons(car, cdr)
        implicit def inject(a: Any): L = a match
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

object SchemeLattice:
    def apply[L, A <: Address](
        implicit lat: SchemeLattice[L, A]
      ): SchemeLattice[L, A] = lat
