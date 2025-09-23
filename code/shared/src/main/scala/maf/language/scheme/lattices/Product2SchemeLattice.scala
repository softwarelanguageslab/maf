package maf.language.scheme.lattices

import maf.core.*
import maf.lattice.interfaces.*
import maf.language.scheme.lattices.*
import maf.language.scheme.lattices.*
import maf.lattice.interfaces.*
import maf.modular.scheme.PtrAddr
import maf.util.MonoidInstances.setMonoid
import maf.util.benchmarks.Table
import maf.util.*
import maf.language.racket.RMod

trait Product2SchemeLattice[L, O, A <: Address] extends SchemeLattice[L, A]:
    def getRight(v: L): O
    def setRight(v: L, o: O): L

/**
 * A product Scheme lattice, where the left behaves like a regular scheme lattice, combined with another lattice O.
 *
 * @tparam O
 *   the other lattice
 */
class Product2ModularSchemeLattice[
    A <: Address,
    S: StringLattice,
    B: BoolLattice,
    I: IntLattice,
    R: RealLattice,
    C: CharLattice,
    Sym: SymbolLattice,
    O: Lattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym]:

    case class PL(left: L, right: O):
        def mapLeft(f: L => L): PL =
            PL(f(left), right)

    def product2Lattice: Product2SchemeLattice[PL, O, A] = new Product2SchemeLattice:
        def show(x: PL): String = x.toString
        def refs(x: PL): Set[Address] = schemeLattice.refs(x.left)
        def isTrue(x: PL): Boolean = schemeLattice.isTrue(x.left)
        def isFalse(x: PL): Boolean = schemeLattice.isFalse(x.left)
        def isBoolean(x: PL): Boolean = schemeLattice.isBoolean(x.left)
        def retractBool(x: PL): PL = PL(schemeLattice.retractBool(x.left), x.right)
        def op(op: SchemeOp)(args: List[PL]): MayFail[PL, Error] =
            schemeLattice.op(op)(args.map(_.left)).map(PL(_, Lattice[O].bottom))

        def join(x: PL, y: => PL): PL =
            PL(schemeLattice.join(x.left, y.left), Lattice[O].join(x.right, y.right))
        def subsumes(x: PL, y: => PL): Boolean =
            schemeLattice.subsumes(x.left, y.left) && Lattice[O].subsumes(x.right, y.right)
        def top: PL = throw LatticeTopUndefined

        def getClosures(x: PL): Set[Closure] = schemeLattice.getClosures(x.left)
        def getContinuations(x: PL): Set[K] = schemeLattice.getContinuations(x.left)
        def getPrimitives(x: PL): Set[String] = schemeLattice.getPrimitives(x.left)
        def getPointerAddresses(x: PL): Set[A] = schemeLattice.getPointerAddresses(x.left)
        def bottom: PL = PL(schemeLattice.bottom, Lattice[O].bottom)

        override def isBottom(x: PL): Boolean = schemeLattice.isBottom(x.left) && Lattice[O].isBottom(x.right)
        def number(x: BigInt): PL = PL(schemeLattice.number(x), Lattice[O].bottom)

        def numTop: PL = PL(schemeLattice.numTop, Lattice[O].bottom)
        def charTop: PL = PL(schemeLattice.charTop, Lattice[O].bottom)
        def realTop: PL = PL(schemeLattice.realTop, Lattice[O].bottom)
        def stringTop: PL = PL(schemeLattice.stringTop, Lattice[O].bottom)
        def symbolTop: PL = PL(schemeLattice.symbolTop, Lattice[O].bottom)
        def real(x: Double): PL = PL(schemeLattice.real(x), Lattice[O].bottom)
        def string(x: String): PL = PL(schemeLattice.string(x), Lattice[O].bottom)
        def char(x: scala.Char): PL = PL(schemeLattice.char(x), Lattice[O].bottom)
        def bool(x: Boolean): PL = PL(schemeLattice.bool(x), Lattice[O].bottom)
        def primitive(x: String): PL = PL(schemeLattice.primitive(x), Lattice[O].bottom)
        def closure(x: Closure): PL = PL(schemeLattice.closure(x), Lattice[O].bottom)
        def cont(x: K): PL = PL(schemeLattice.cont(x), Lattice[O].bottom)
        def symbol(x: String): PL = PL(schemeLattice.symbol(x), Lattice[O].bottom)
        def cons(car: PL, cdr: PL): PL = PL(schemeLattice.cons(car.left, cdr.left), Lattice[O].bottom)
        def pointer(a: A): PL = PL(schemeLattice.pointer(a), Lattice[O].bottom)
        def nil: PL = PL(schemeLattice.nil, Lattice[O].bottom)
        def void: PL = PL(schemeLattice.void, Lattice[O].bottom)

        def eql[B2: BoolLattice](x: PL, y: PL): B2 = schemeLattice.eql(x.left, y.left) // TODO also incoperate the right hand side of the lattice
        def eq(xs: PL, ys: PL)(cmp: MaybeEq[A]): PL = PL(schemeLattice.eq(xs.left, ys.left)(cmp), Lattice[O].join(xs.right, ys.right))

        def getRight(v: PL): O = v.right
        def setRight(v: PL, o: O): PL = PL(v.left, o)

        def rmods(mod: PL): Set[RMod[PL]] = schemeLattice.rmods(mod.left).map(_.mapValues(PL(_, Lattice[O].bottom)))
        def rmod(mod: RMod[PL]): PL =
            PL(schemeLattice.rmod(mod.mapValues(_.left)), Lattice[O].bottom)
