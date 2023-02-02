package maf.modular.incremental.scheme.lattice

import maf.core.*
import maf.lattice.HMap
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

/** A modular Scheme lattice that also provides operations on address-annotated values. */
class IncrementalModularSchemeLattice[
    A <: Address,
    S: StringLattice,
    B: BoolLattice,
    I: IntLattice,
    R: RealLattice,
    C: CharLattice,
    Sym: SymbolLattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym]:

    import Elements.*

    type Sources = Set[A]

    /**
     * * A value that keeps track of the store addresses it depends upon.
     * @param values
     *   The list of abstract values.
     * @param sources
     *   The addresses in the store where the value originated.
     */
    case class AnnotatedElements(value: L, sources: Sources) extends SmartHash:
        override def toString: String = toL().toString

        /** Convert to a non-annotated value, for use with the non-annotated lattice implementation. */
        def toL(): L = value
        def joinedSources(other: AnnotatedElements): Sources = sources.union(other.sources) //TODO use a set monoid for this?

    type AL = AnnotatedElements
    object AnnotatedElement extends Serializable:
        def apply(v: Value): AL = apply(v, Set())
        def apply(v: Value, sources: Sources): AL =
            // NOTE: the usage of asInstanceOf here is slightly unsafe since Value.tpy.Wrap might not correspond to the actual type of v
            // we trust the programmers to not make this mistake, but it cannot be enforcement at compile-time.
            // Removal of these wrappers might (partially) solve the problem.
            AnnotatedElements(HMap.wrapInserted(v.tpy, v.asInstanceOf[v.tpy.Wrap]), sources)

    def toAL(x: L): AL = AnnotatedElements(x, Set())

    import maf.util.MonoidInstances._

    /** Provides the monoid implementation for annotated elements AL. */
    implicit val alMonoid: Monoid[AL] = new Monoid[AL] {
        def append(xs: AL, ys: => AL): AL = AnnotatedElements(Lattice[L].join(xs.toL(), ys.toL()), xs.sources.union(ys.sources))
        def zero: AL = AnnotatedElements(Lattice[L].bottom, Set())
    }
    implicit val alMFMonoid: Monoid[MayFail[AL, Error]] = MonoidInstances.mayFail[AL]

    /** The actual lattice implementation for AL and A. */
    val incrementalSchemeLattice: IncrementalSchemeLattice[AL, A] = new IncrementalSchemeLattice[AL, A] {

        /** Converts an elements from the non-annotated lattice to the annotated lattice. */
        private def annotate(als: L, sources: Sources): AL = AnnotatedElements(als, sources)

        def show(x: AL): String = x.toString /* TODO[easy]: implement better */
        def refs(x: AL): Set[Address] = schemeLattice.refs(x.toL())
        def isTrue(x: AL): Boolean = schemeLattice.isTrue(x.toL())
        def isFalse(x: AL): Boolean = schemeLattice.isFalse(x.toL())
        def isBoolean(x: AL): Boolean = schemeLattice.isBoolean(x.toL())
        def retractBool(x: AL): AL =
            annotate(schemeLattice.retractBool(x.toL()), x.sources)

        def isOpq(x: AL): Boolean = schemeLattice.isOpq(x.toL())

        /** Apply an operation: apply the operation on the values and join the annotations. */
        def op(op: SchemeOp)(args: List[AL]): MayFail[AL, Error] =
            val sources = args.flatMap(_.sources).toSet // Combine all sources (= join).
            // Redirect to the non-annotated lattice and annotate afterwards. This allows more reuse of code.
            schemeLattice.op(op)(args.map(_.toL())).map(annotate(_, sources))
        def join(x: AL, y: => AL): AL =
            val ys =
                y // Breaks laziness: we want y to be evaluated exactly once. If not, y will be evaluates twice (once to get the value and once to get the sources).
            AnnotatedElements(schemeLattice.join(x.toL(), ys.toL()), x.sources.union(ys.sources))
        // Monoid[AL].append(x, y)
        /** Sumsumption check. Returns true if x ⊒ y and false otherwise. */
        def subsumes(x: AL, y: => AL): Boolean = schemeLattice.subsumes(x.toL(), y.toL())
        def top: AL = throw LatticeTopUndefined

        def getClosures(x: AL): Set[Closure] = schemeLattice.getClosures(x.toL())
        def getContinuations(x: AL): Set[K] = schemeLattice.getContinuations(x.toL())
        def getPrimitives(x: AL): Set[String] = schemeLattice.getPrimitives(x.toL())
        def getPointerAddresses(x: AL): Set[A] = schemeLattice.getPointerAddresses(x.toL())
        def getThreads(x: AL): Set[TID] = schemeLattice.getThreads(x.toL())
        def getBlames(x: AL): Set[Blame] = schemeLattice.getBlames(x.toL())
        def getGrds(x: AL): Set[Grd[AL]] =
            schemeLattice.getGrds(x.toL()).map(_.map(annotate(_, Set()))) // TODO[medium] not sure what to pass to annotate
        def getArrs(x: AL): Set[Arr[AL]] =
            schemeLattice.getArrs(x.toL()).map(_.map(annotate(_, Set()))) // TODO[medium] not sure what to pass to annotate
        def getFlats(x: AL): Set[Flat[AL]] =
            schemeLattice.getFlats(x.toL()).map(_.map(annotate(_, Set()))) // TODO[medium] not sure what to pass to annotate
        def getGetterSetter(x: AL): Set[StructSetterGetter] =
            schemeLattice.getGetterSetter(x.toL())
        def getStructs(x: AL): Set[Struct[AL]] =
            schemeLattice.getStructs(x.toL()).map(_.map(annotate(_, Set())))
        def getStructConstructor(x: AL): Set[StructConstructor] =
            schemeLattice.getStructConstructor(x.toL())
        def getStructPredicates(x: AL): Set[StructPredicate] =
            schemeLattice.getStructPredicates(x.toL())

        def acquire(lock: AL, tid: TID): MayFail[AL, Error] = schemeLattice.acquire(lock.toL(), tid).map(annotate(_, lock.sources))
        def release(lock: AL, tid: TID): MayFail[AL, Error] = schemeLattice.release(lock.toL(), tid).map(annotate(_, lock.sources))

        def bottom: AL = AnnotatedElements(schemeLattice.bottom, Set())

        override def isBottom(x: AL): Boolean = removeAddresses(x) == bottom // Just check the value.

        def number(x: BigInt): AL = toAL(schemeLattice.number(x))

        def numTop: AL = toAL(schemeLattice.numTop)
        def charTop: AL = toAL(schemeLattice.charTop)
        def realTop: AL = toAL(schemeLattice.realTop)
        def stringTop: AL = toAL(schemeLattice.stringTop)
        def symbolTop: AL = toAL(schemeLattice.symbolTop)
        def real(x: Double): AL = toAL(schemeLattice.real(x))
        def string(x: String): AL = toAL(schemeLattice.string(x))
        def char(x: scala.Char): AL = toAL(schemeLattice.char(x))
        def bool(x: Boolean): AL = toAL(schemeLattice.bool(x))
        def primitive(x: String): AL = toAL(schemeLattice.primitive(x))
        def closure(x: Closure): AL = toAL(schemeLattice.closure(x))
        def cont(x: K): AL = toAL(schemeLattice.cont(x))
        def symbol(x: String): AL = toAL(schemeLattice.symbol(x))
        def cons(car: AL, cdr: AL): AL = AnnotatedElements(schemeLattice.cons(car.toL(), cdr.toL()), car.joinedSources(cdr))
        def pointer(a: A): AL = toAL(schemeLattice.pointer(a))
        def thread(tid: TID): AL = toAL(schemeLattice.thread(tid))
        def lock(threads: Set[TID]): AL = toAL(schemeLattice.lock(threads))
        def blame(blame: Blame): AL = toAL(schemeLattice.blame(blame))
        def grd(grd: Grd[AL]): AL = toAL(schemeLattice.grd(grd.map(_.toL()))) // TODO: grd looses its annotation if used like this?
        def arr(arr: Arr[AL]): AL = toAL(schemeLattice.arr(arr.map(_.toL())))
        def flat(flt: Flat[AL]): AL = toAL(schemeLattice.flat(flt.map(_.toL())))
        def struct(struct: Struct[AL]): AL = toAL(schemeLattice.struct(struct.map(_.toL())))
        def structConstructor(constr: StructConstructor): AL = toAL(schemeLattice.structConstructor(constr))
        def structPredicate(pred: StructPredicate): AL = toAL(schemeLattice.structPredicate(pred))
        def structSetterGetter(setterGetter: StructSetterGetter): AL =
            toAL(schemeLattice.structSetterGetter(setterGetter))
        def opq(opq: Opq): AL = toAL(schemeLattice.opq(opq))
        def nil: AL = toAL(schemeLattice.nil)
        def void: AL = toAL(schemeLattice.void)
        def eql[B2: BoolLattice](x: AL, y: AL): B2 = schemeLattice.eql(x.toL(), y.toL())(BoolLattice[B2])
        def eq(xs: AL, ys: AL)(cmp: MaybeEq[A]): AL = annotate(schemeLattice.eq(xs.toL(), ys.toL())(cmp), xs.joinedSources(ys))

        override def rmods(mod: AL): Set[RMod[AL]] =
            schemeLattice.rmods(mod.toL()).map(_.mapValues(annotate(_, Set())))

        override def rmod(mod: RMod[AL]): AL =
            annotate(schemeLattice.rmod(mod.mapValues(_.toL())), Set())

        override def addAddresses(v: AL, addresses: Sources): AL = AnnotatedElements(v.toL(), v.sources.union(addresses))
        override def getAddresses(v: AL): Set[A] = v.sources
        override def removeAddresses(v: AL): AL = v.copy(sources = Set())

        /** Shows the differences between x and y (from the perspective of a modular lattice). */
        def compare(
            x: AL,
            y: AL,
            xname: String = "x",
            yname: String = "y"
          ): String =
            var table = Table.empty.withDefaultValue("")
            def insertList(name: String, values: List[Value]): Unit =
                values.foreach { v =>
                    if v.ord == 9 then // Pointers.
                        val ptrs = v
                            .asInstanceOf[Pointer]
                            .ptrs
                            .map(addr => {
                                val poi = addr.asInstanceOf[PtrAddr[_]]
                                s"${poi.exp.idn.pos.toString} [${poi.ctx}]"
                            })
                            .toList
                            .sorted
                            .mkString("Pointer(", ", ", ")")
                        table = table.add(v.typeName, name, ptrs)
                    else table = table.add(v.typeName, name, v.toString)
                }
            insertList(xname, x.value.vs)
            insertList(yname, y.value.vs)
            (x.value.vs.diff(y.value.vs) ++ y.value.vs.diff(x.value.vs)).map(_.typeName).foreach { typeName =>
                table = table.add(typeName, "diff", "!")
            }
            table.prettyString(columns = List("diff", xname, yname))
    }

    object AL:
        implicit val lattice: IncrementalSchemeLattice[AL, A] = incrementalSchemeLattice
