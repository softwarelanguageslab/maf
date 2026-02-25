package maf.language.scheme.lattices

import maf.core.*
import maf.lattice.interfaces.*
import maf.language.scheme.lattices.*
import maf.language.CScheme.TID
import maf.language.ContractScheme.ContractValues.*
import maf.language.scheme.lattices.*
import maf.lattice.interfaces.*
import maf.modular.scheme.PtrAddr
import maf.util.MonoidInstances.setMonoid
import maf.util.benchmarks.Table
import maf.util.*
import maf.language.AScheme.ASchemeValues.*
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
        def level(v: PL): scala.Int = Lattice[L].level(v.left) * Lattice[O].level(v.right)
        def show(x: PL): String = x.toString
        def refs(x: PL): Set[Address] = schemeLattice.refs(x.left)
        def isTrue(x: PL): Boolean = schemeLattice.isTrue(x.left)
        def isFalse(x: PL): Boolean = schemeLattice.isFalse(x.left)
        def isBoolean(x: PL): Boolean = schemeLattice.isBoolean(x.left)
        def retractBool(x: PL): PL = PL(schemeLattice.retractBool(x.left), x.right)
        def isOpq(x: PL): Boolean = schemeLattice.isOpq(x.left)
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
        def getThreads(x: PL): Set[TID] = schemeLattice.getThreads(x.left)
        def getBlames(x: PL): Set[Blame] = schemeLattice.getBlames(x.left)
        def getGrds(x: PL): Set[Grd[PL]] =
            schemeLattice.getGrds(x.left).map(_.map(PL(_, Lattice[O].bottom)))

        def getArrs(x: PL): Set[Arr[PL]] =
            schemeLattice.getArrs(x.left).map(_.map(PL(_, Lattice[O].bottom)))

        def getFlats(x: PL): Set[Flat[PL]] =
            schemeLattice.getFlats(x.left).map(_.map(PL(_, Lattice[O].bottom)))

        def getGetterSetter(x: PL): Set[StructSetterGetter] =
            schemeLattice.getGetterSetter(x.left)

        def getStructs(x: PL): Set[Struct[PL]] =
            schemeLattice.getStructs(x.left).map(_.map(PL(_, Lattice[O].bottom)))

        def getStructConstructor(x: PL): Set[StructConstructor] =
            schemeLattice.getStructConstructor(x.left)

        def getStructPredicates(x: PL): Set[StructPredicate] =
            schemeLattice.getStructPredicates(x.left)

        def acquire(lock: PL, tid: TID): MayFail[PL, Error] = schemeLattice.acquire(lock.left, tid).map(PL(_, Lattice[O].bottom))

        def release(lock: PL, tid: TID): MayFail[PL, Error] = schemeLattice.release(lock.left, tid).map(PL(_, Lattice[O].bottom))

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
        def thread(tid: TID): PL = PL(schemeLattice.thread(tid), Lattice[O].bottom)
        def lock(threads: Set[TID]): PL = PL(schemeLattice.lock(threads), Lattice[O].bottom)
        def blame(blame: Blame): PL = PL(schemeLattice.blame(blame), Lattice[O].bottom)
        def grd(grd: Grd[PL]): PL = PL(schemeLattice.grd(grd.map(_.left)), Lattice[O].bottom)
        def arr(arr: Arr[PL]): PL = PL(schemeLattice.arr(arr.map(_.left)), Lattice[O].bottom)
        def flat(flt: Flat[PL]): PL = PL(schemeLattice.flat(flt.map(_.left)), Lattice[O].bottom)
        def struct(struct: Struct[PL]): PL = PL(schemeLattice.struct(struct.map(_.left)), Lattice[O].bottom)
        def structConstructor(constr: StructConstructor): PL = PL(schemeLattice.structConstructor(constr), Lattice[O].bottom)
        def structPredicate(pred: StructPredicate): PL = PL(schemeLattice.structPredicate(pred), Lattice[O].bottom)
        def structSetterGetter(setterGetter: StructSetterGetter): PL =
            PL(schemeLattice.structSetterGetter(setterGetter), Lattice[O].bottom)
        def opq(opq: Opq): PL = PL(schemeLattice.opq(opq), Lattice[O].bottom)
        def nil: PL = PL(schemeLattice.nil, Lattice[O].bottom)
        def void: PL = PL(schemeLattice.void, Lattice[O].bottom)

        def eql[B2: BoolLattice](x: PL, y: PL): B2 = schemeLattice.eql(x.left, y.left) // TODO also incoperate the right hand side of the lattice
        def eq(xs: PL, ys: PL)(cmp: MaybeEq[A]): PL = PL(schemeLattice.eq(xs.left, ys.left)(cmp), Lattice[O].join(xs.right, ys.right))

        def getRight(v: PL): O = v.right
        def setRight(v: PL, o: O): PL = PL(v.left, o)

        def rmods(mod: PL): Set[RMod[PL]] = schemeLattice.rmods(mod.left).map(_.mapValues(PL(_, Lattice[O].bottom)))
        def rmod(mod: RMod[PL]): PL =
            PL(schemeLattice.rmod(mod.mapValues(_.left)), Lattice[O].bottom)
