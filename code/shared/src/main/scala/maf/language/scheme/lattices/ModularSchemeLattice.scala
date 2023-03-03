package maf.language.scheme.lattices

import maf.language.racket.*
import maf.core._
import maf.lattice.{AbstractSetType, AbstractType, AbstractWrapType, HMap, HMapKey, NoExtract, NoInject}
import maf.util.MonoidImplicits.*
import maf.language.CScheme.TID
import maf.language.ContractScheme.ContractValues._
import maf.language.AScheme.ASchemeValues.{Actor, Behavior}
import maf.language.scheme.primitives.SchemePrimitive
import maf.lattice.interfaces._
import maf.util.datastructures.SmartUnion._
import maf.util._
import smtlib.theories.Core.False
import maf.language.AScheme.ASchemeValues.Future
import maf.language.AScheme.ASchemeLattice

/**
 * Defines a Scheme lattice based on other lattices. Example usage: val address = NameAddress val lattice = new ModularSchemeLattice[SchemeExp,
 * address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym] Now `lattice.L` is a SchemeLattice, of which the implicit for the typeclass is
 * available in the current scope.
 */
/** TODO[medium]: use Show and ShowStore here */
class ModularSchemeLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends Serializable
    with SchemeLattice[HMap, A]:

    trait ModularSchemeConvertor[BaseAddr <: Address, V]:
        def convertAddr(addr: Address): BaseAddr
        def convertValue(baseDomain: ModularSchemeLattice[A, S, B, I, R, C, Sym])(v: V): baseDomain.L

    // TODO: make this a type parameter for type safety!
    type K = Any

    type P = SchemePrimitive[L, A]

    implicit def mfAddrMonoid[X]: Monoid[MayFail[Set[X], Error]] = MonoidImplicits.mayFail[Set[X]](MonoidImplicits.setMonoid[X])

    object StrT extends AbstractWrapType[S, Str]:
        type Inject = String
        override def inject(v: Inject): Abstract = StringLattice[S].inject(v)
        def wrap = Str.apply

    object BoolT extends AbstractWrapType[B, Bool]:
        type Inject = Boolean
        override def inject(v: Inject): Abstract = BoolLattice[B].inject(v)
        def wrap = Bool.apply

    object IntT extends AbstractWrapType[I, Int]:
        type Inject = BigInt
        override def inject(v: Inject): Abstract = IntLattice[I].inject(v)
        def wrap = Int.apply

    object RealT extends AbstractWrapType[R, Real]:
        type Inject = Double
        override def inject(v: Inject): Abstract = RealLattice[R].inject(v)
        def wrap = Real.apply

    object CharT extends AbstractWrapType[C, Char]:
        type Inject = scala.Char
        override def inject(v: scala.Char): Abstract = CharLattice[C].inject(v)
        def wrap = Char.apply

    object SymbolT extends AbstractWrapType[Sym, Symbol]:
        type Inject = String
        override def inject(v: String): Abstract = SymbolLattice[Sym].inject(v)
        def wrap = Symbol.apply

    object PrimT extends AbstractSetType[String, Prim]:
        def wrap = Prim.apply

    object CloT extends AbstractSetType[schemeLattice.Closure, Clo]:
        def wrap = Clo.apply

    object NilT extends AbstractType[Unit, Nil.type]:
        type Extract = NoExtract
        type Inject = NoInject
        def wrap = (_) => Nil
        def unwrap(v: Wrap) = ()
        val lattice: Lattice[Abstract] = Lattice.UnitLattice

    object PointerT extends AbstractSetType[A, Pointer]:
        def wrap = Pointer.apply

    object ConsT extends AbstractType[(L, L), Cons]:
        type Extract = NoExtract
        type Inject = NoInject

        def wrap = Cons.apply.tupled
        def unwrap(v: Wrap) = (v.car, v.cdr)

        // TODO: this in ad-hoc lattice, we should make a PairLattice instead somewhere
        val lattice: Lattice[Abstract] = new Lattice[Abstract] {
            def show(v: (L, L)): String = s"(${v._1},${v._2})"
            def top = throw LatticeTopUndefined
            def bottom = ???
            def join(x: (L, L), y: => (L, L)): (L, L) = (schemeLattice.join(x._1, y._1), schemeLattice.join(x._2, y._2))
            def subsumes(x: (L, L), y: => (L, L)): Boolean =
                (schemeLattice.subsumes(x._1, y._1) && schemeLattice.subsumes(x._2, y._2))
            def eql[B: BoolLattice](x: (L, L), y: (L, L)): B = ???
        }

    object VecT extends AbstractType[Vec, Vec]:
        type Inject = NoInject
        type Extract = Vec
        def wrap = (v) => v
        def unwrap(v: Vec) = v
        override def extract(v: Vec) = v
        // TODO: ad-hoc implementation, should create a VectorLattice
        val lattice: Lattice[Vec] = new Lattice[Vec] {
            def show(v: Vec) = v.toString
            def top = throw LatticeTopUndefined
            def bottom = Vec(IntLattice[I].bottom, Map())
            def join(x: Vec, y: => Vec): Vec =
                val size1 = x.size
                val size2 = y.size
                val els1 = x.elements
                val els2 = y.elements
                // First, joins the size
                val vSizeInitJoined = Vec(IntLattice[I].join(size1, size2), Map.empty)
                // Then, joins elements by adding (with vector-set!) all elements of els1 and then els2 inside the new vector
                val vWithEls1Joined = els1.foldLeft(vSizeInitJoined)({ case (acc, (k, v)) =>
                    // potentially unsafe, should really be improved
                    Value
                        .vectorSet(acc, Int(k), v)
                        .map(v => v.get(VecT)(using Default.errorIfDefault))
                        .getOrElse(throw new Exception("operation failed"))
                })
                val vWithEls2Joined = els2.foldLeft(vWithEls1Joined)({ case (acc, (k, v)) =>
                    // potentially unsafe, should really be improved
                    Value
                        .vectorSet(acc, Int(k), v)
                        .map(v => v.get(VecT)(using Default.errorIfDefault))
                        .getOrElse(throw new Exception("operation failed"))
                })
                vWithEls2Joined
            def subsumes(x: Vec, y: => Vec): Boolean =
                val siz1 = x.size
                val siz2 = y.size
                val els1 = x.elements
                val els2 = y.elements
                IntLattice[I].subsumes(siz1, siz2) &&
                els2.forall { case (idx2, vlu2) =>
                    els1.exists { case (idx1, vlu1) =>
                        IntLattice[I].subsumes(idx1, idx2) && schemeLattice.subsumes(vlu1, vlu2)
                    }
                }
            def eql[B: BoolLattice](x: Vec, y: Vec): B = ???
        }

    object KontT extends AbstractSetType[K, Kont]:
        def wrap = Kont.apply

    object ThreadT extends AbstractSetType[TID, Thread]:
        def wrap = Thread.apply

    object LockT extends AbstractSetType[TID, Lock]:
        def wrap = Lock.apply

    object VoidT extends AbstractType[Unit, Void.type]:
        type Extract = NoExtract
        type Inject = NoInject
        def wrap = (_) => Void
        def unwrap(v: Wrap): Abstract = ()
        val lattice: Lattice[Abstract] = Lattice.UnitLattice

    object InputPortT extends AbstractWrapType[L, InputPort](using L.lattice):
        type Extract = NoExtract
        type Inject = NoInject
        def wrap = InputPort.apply

    object OutputPortT extends AbstractWrapType[L, OutputPort](using L.lattice):
        type Extract = NoExtract
        type Inject = NoInject
        def wrap = OutputPort.apply

    object BlameT extends AbstractSetType[Blame, Blames]:
        def wrap = Blames.apply

    object ArrT extends AbstractSetType[Arr[L], Arrs]:
        def wrap = Arrs.apply

    object GrdT extends AbstractSetType[Grd[L], Grds]:
        def wrap = Grds.apply

    object Arrs extends AbstractSetType[Arr[L], Arrs]:
        def wrap = Arrs.apply

    object FlatT extends AbstractSetType[Flat[L], Flats]:
        def wrap = Flats.apply

    object OpqT extends AbstractSetType[Opq, Opqs]:
        def wrap = Opqs.apply

    object StructT extends AbstractSetType[Struct[L], Structs]:
        def wrap = Structs.apply

    object StructSetterGetterT extends AbstractSetType[StructSetterGetter, StructSetterGetters]:
        def wrap = StructSetterGetters.apply

    object StructConstructorT extends AbstractSetType[StructConstructor, StructConstructors]:
        def wrap = StructConstructors.apply

    object StructPredicateT extends AbstractSetType[StructPredicate, StructPredicates]:
        def wrap = StructPredicates.apply

    /**
     * We first implement all possible operations on single values, that can be only joined when compatible. This therefore is not a lattice but will
     * be used to build the set lattice
     */
    trait Value extends SmartHash:
        def ord: scala.Int
        val tpy: HMapKey
        def typeName: String // Can be used to print information on values.

    case class Str(s: S) extends Value, Product1[S]:
        def ord = 0
        val tpy = StrT
        def typeName = "STRG"
        override def toString: String = StringLattice[S].show(s)

    case class Bool(b: B) extends Value, Product1[B]:
        def ord = 1
        def typeName = "BOOL"
        val tpy = BoolT
        override def toString: String = BoolLattice[B].show(b)

    case class Int(i: I) extends Value, Product1[I]:
        def ord = 2
        def typeName = "INTE"
        val tpy = IntT
        override def toString: String = IntLattice[I].show(i)

    case class Real(r: R) extends Value, Product1[R]:
        def ord = 3
        def typeName = "REAL"
        val tpy = RealT
        override def toString: String = RealLattice[R].show(r)
    case class Char(c: C) extends Value, Product1[C]:
        def ord = 4
        def typeName = "CHAR"
        val tpy = CharT
        override def toString: String = CharLattice[C].show(c)
    case class Symbol(s: Sym) extends Value, Product1[Sym]:
        def ord = 5
        def typeName = "SYMB"
        val tpy = SymbolT
        override def toString: String = SymbolLattice[Sym].show(s)
    case class Prim(prims: Set[String]) extends Value, Product1[Set[String]]:
        def ord = 6
        def typeName = "PRIM"
        val tpy = PrimT
        override def toString: String = prims.mkString("Primitive{", ", ", "}")
    // TODO: define `type Closure = (SchemeLambdaExp, Env)` (maybe using a case class)
    case class Clo(closures: Set[schemeLattice.Closure]) extends Value, Product1[Set[schemeLattice.Closure]]:
        def ord = 7
        def typeName = "CLOS"
        val tpy = CloT
        override def toString: String =
            closures
                .map(_._1.lambdaName)
                .mkString("Closures{", ", ", "}")
    case object Nil extends Value:
        def ord = 8
        def typeName = "NULL"
        val tpy = NilT
        override def toString: String = "()"

    case class Pointer(ptrs: Set[A]) extends Value, Product1[Set[A]]:
        def ord = 9
        def typeName = "PNTR"
        val tpy = PointerT
        override def toString: String =
            if ptrs.size > 3
            then s"{${ptrs.size} pointers}"
            else ptrs.mkString("Pointers{", ", ", "}")

    case class Cons(car: L, cdr: L) extends Value:
        def ord = 10
        def typeName = "CONS"
        val tpy = ConsT
        override def toString: String = s"($car . $cdr)"

    case class Vec(size: I, elements: Map[I, L]) extends Value:
        def ord = 11
        def typeName = "VECT"
        val tpy = VecT
        override def toString: String =
            val els = elements.toList
                .map({ case (k, v) =>
                    s"$k: $v"
                })
                .mkString(", ")
            s"Vector(size: $size, elems: {$els})"

    case class Kont(k: Set[K]) extends Value, Product1[Set[K]]:
        def ord = 12
        def typeName = "KONT"
        val tpy = KontT
        override def toString: String = "<continuation>"
    case class Thread(threads: Set[TID]) extends Value, Product1[Set[TID]]:
        def ord = 13
        val tpy = ThreadT
        def typeName = "THRD"
        override def toString: String = threads.mkString("Thread{", ", ", "}")
    // Could also store (a) Thread(s) here, but this seems to be simpler.
    // An empty set indicates the lock is not held, but a non-empty set may also indicate this... (due to the monotonicity of the analysis, threads will only increase in size).
    // This should correspond to the formalisation of ModConc and \lambda_\tau.
    case class Lock(threads: Set[TID]) extends Value, Product1[Set[TID]]:
        def ord = 14
        def typeName = "LOCK"
        val tpy = LockT
        override def toString: String = threads.mkString("Lock{", ", ", "}")

    case object Void extends Value:
        def ord = 15
        def typeName = "VOID"
        val tpy = VoidT
        override def toString: String = "<void>"
    case class InputPort(id: L) extends Value, Product1[L]:
        def ord = 16
        def typeName = "IPRT"
        val tpy = InputPortT
        override def toString: String = s"InputPort{$id}"
    case class OutputPort(id: L) extends Value, Product1[L]:
        def ord = 17
        def typeName = "OPRT"
        val tpy = OutputPortT
        override def toString: String = s"OutputPort{$id}"

    case class Blames(blames: Set[Blame]) extends Value, Product1[Set[Blame]]:
        def ord = 18
        def typeName = "BLAME"
        val tpy = BlameT
        override def toString: String = s"<blame: ${blames.map(_.toString).mkString(",")}>"

    case class Grds(grds: Set[Grd[L]]) extends Value, Product1[Set[Grd[L]]]:
        def ord = 19
        def typeName = "GRD"
        val tpy = GrdT
        override def toString: String = s"<grd: ${grds.map(_.toString).mkString(",")}>"

    case class Arrs(arrs: Set[Arr[L]]) extends Value, Product1[Set[Arr[L]]]:
        def ord = 20
        def typeName = "ARR"
        val tpy = ArrT
        override def toString: String = s"<arr: ${arrs.map(_.toString).mkString(",")}>"

    case class Flats(flats: Set[Flat[L]]) extends Value, Product1[Set[Flat[L]]]:
        def ord = 21
        def typeName = "FLT"
        val tpy = FlatT
        override def toString: String = s"<flat: ${flats.map(_.toString).mkString(",")}>"

    case class Opqs(opqs: Set[Opq]) extends Value, Product1[Set[Opq]]:
        def ord = 22
        def typeName = "OPQ"
        val tpy = OpqT
        override def toString: String = s"<opq>"

    case class Structs(structs: Set[Struct[L]]) extends Value, Product1[Set[Struct[L]]]:
        def ord = 23
        def typeName = "STRUCT"
        val tpy = StructT
        override def toString: String = s"<struct: ${structs.map(_.tag).mkString(",")}>"

    case class StructSetterGetters(getterSetters: Set[StructSetterGetter]) extends Value, Product1[Set[StructSetterGetter]]:
        def ord = 24
        def typeName = "STRUCTGETTERSETTER"
        val tpy = StructSetterGetterT
        override def toString: String = "<struct-getter-setter>"

    case class StructConstructors(constructors: Set[StructConstructor]) extends Value, Product1[Set[StructConstructor]]:
        def ord = 25
        def typeName = "STRUCTCONSTRUCTOR"
        val tpy = StructConstructorT
        override def toString: String = "<struct-constructor>"

    case class StructPredicates(predicates: Set[StructPredicate]) extends Value, Product1[Set[StructPredicate]]:
        def ord = 26
        def typeName = "STRUCTPREDICATE"
        val tpy = StructPredicateT
        override def toString: String = "<struct-predicate>"

    /** The injected true value */
    val True: Bool = Bool(BoolLattice[B].inject(true))

    /** The injected false value */
    val False: Bool = Bool(BoolLattice[B].inject(false))

    object RModT extends AbstractSetType[RMod[HMap], RMods]:
        def wrap = RMods.apply

    case class RMods(mods: Set[RMod[HMap]]) extends Value, Product1[Set[RMod[HMap]]]:
        def ord = 34
        def typeName: String = "RMOD"
        val tpy = RModT
        override def toString(): String = s"<mod: $mods>"

    object Value:
        // TODO: Opq has a special status for subsumes, in that it subsumes everything (i.e. it is like top)

        def isTrue(x: Value): Boolean = x match
            case Bool(b) => BoolLattice[B].isTrue(b)
            case _       => true
        def isFalse(x: Value): Boolean = x match
            case Bool(b) => BoolLattice[B].isFalse(b)
            case Opqs(_) => true
            case _       => false
        def isBoolean(x: Value): Boolean = x match
            case Bool(_) => true
            case _       => false
        def isOpq(x: Value): Boolean = x match
            case Opqs(_) => true
            case _       => false

        def op(op: SchemeOp)(args: List[Value]): MayFail[Value, Error] =
            import SchemeOp._
            op.checkArity(args)
            op match
                case Car        => throw new Exception("ModularSchemeLattice: car SchemeOp not supported on Value")
                case Cdr        => throw new Exception("ModularSchemeLattice: cdr SchemeOp not supported on Value")
                case MakeVector => throw new Exception("ModularSchemeLattice: make-vector SchemeOp not supported on Value")
                case VectorRef  => throw new Exception("ModularSchemeLattice: vector-ref SchemeOp not supported on Value")
                case VectorSet  => throw new Exception("ModularSchemeLattice: vector-set SchemeOp not supported on Value")

                case IsNull =>
                    MayFail.success(args(0) match {
                        case Nil => True
                        case _   => False
                    })

                case _: TypeOp if isOpq(args(0)) =>
                    MayFail.success(Bool(BoolLattice[B].top))

                case IsCons =>
                    MayFail.success(args(0) match {
                        case _: Cons => True
                        case _       => False
                    })
                case IsPointer =>
                    MayFail.success(args(0) match {
                        case _: Pointer => True
                        case _          => False
                    })
                case IsChar =>
                    MayFail.success(args(0) match {
                        case _: Char => True
                        case _       => False
                    })
                case IsSymbol =>
                    MayFail.success(args(0) match {
                        case _: Symbol => True
                        case _         => False
                    })
                case IsString =>
                    MayFail.success(args(0) match {
                        case _: Str => True
                        case _      => False
                    })
                case IsInteger =>
                    MayFail.success(args(0) match {
                        case _: Int => True
                        case _      => False
                    })
                case IsReal =>
                    MayFail.success(args(0) match {
                        case _: Real => True
                        case _: Int  => True
                        case _       => False
                    })
                case IsBoolean =>
                    MayFail.success(args(0) match {
                        case _: Bool => True

                        case _ => False
                    })
                case IsNumber =>
                    MayFail.success(args(0) match {
                        case (_: Real) | (_: Int) => True
                        case _                    => False
                    })

                case IsTrue =>
                    MayFail.success(BoolT.alpha(isTrue(args(0))))
                case IsFalse =>
                    MayFail.success(BoolT.alpha(isFalse(args(0))))
                case IsVector =>
                    MayFail.success(args(0) match {
                        case _: Vec => True
                        case _      => False
                    })
                case IsThread =>
                    MayFail.success(args(0) match {
                        case _: Thread => True
                        case _         => False
                    })
                case IsLock =>
                    MayFail.success(args(0) match {
                        case _: Lock => True
                        case _       => False
                    })
                case IsProcedure =>
                    MayFail.success(args(0) match {
                        case _: Clo  => True
                        case _: Prim => True
                        case _       => False
                    })
                case IsInputPort =>
                    MayFail.success(args(0) match {
                        case _: InputPort => True
                        case _            => False
                    })
                case IsOutputPort =>
                    MayFail.success(args(0) match {
                        case _: OutputPort => True
                        case _             => False
                    })
                case Not =>
                    MayFail.success(args(0) match {
                        case Bool(b) => Bool(BoolLattice[B].not(b))
                        case _       => False /* any value is true */
                    })
                case Ceiling =>
                    args(0) match
                        case Int(n)  => MayFail.success(Int(n))
                        case Real(n) => MayFail.success(Real(RealLattice[R].ceiling(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("ceiling", args))
                case Floor =>
                    args(0) match
                        case Int(n)  => MayFail.success(Int(n))
                        case Real(n) => MayFail.success(Real(RealLattice[R].floor(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("floor", args))
                case Round =>
                    args(0) match
                        case Int(n)  => MayFail.success(Int(n))
                        case Real(n) => MayFail.success(Real(RealLattice[R].round(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("round", args))
                case Log =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].log(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].log(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("log", args))
                case Random =>
                    args(0) match
                        case Int(n)  => MayFail.success(Int(IntLattice[I].random(n)))
                        case Real(n) => MayFail.success(Real(RealLattice[R].random(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("random", args))
                case Sin =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].sin(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].sin(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("sin", args))
                case ASin =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].asin(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].asin(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("asin", args))
                case Cos =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].cos(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].cos(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("cos", args))
                case ACos =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].acos(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].acos(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("acos", args))
                case Tan =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].tan(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].tan(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("tan", args))
                case ATan =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].atan(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].atan(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("atan", args))
                case Sqrt =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(RealLattice[R].sqrt(IntLattice[I].toReal(n))))
                        case Real(n) => MayFail.success(Real(RealLattice[R].sqrt(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("sqrt", args))
                case VectorLength =>
                    args(0) match
                        case Vec(size, _) => MayFail.success(Int(size))
                        case _            => MayFail.failure(OperatorNotApplicable("vector-length", args))
                case StringLength =>
                    args(0) match
                        case Str(s) => MayFail.success(Int(StringLattice[S].length(s)))
                        case _      => MayFail.failure(OperatorNotApplicable("string-length", args))
                case NumberToString =>
                    args(0) match
                        case Int(n)  => MayFail.success(Str(IntLattice[I].toString(n)))
                        case Real(n) => MayFail.success(Str(RealLattice[R].toString(n)))
                        case _       => MayFail.failure(OperatorNotApplicable("number->string", args))
                case StringToNumber =>
                    args(0) match
                        // TODO: string may also be a float!
                        case Str(s) => StringLattice[S].toNumber(s).map(Int.apply)
                        case _      => MayFail.failure(OperatorNotApplicable("string->number", args))
                case IntegerToCharacter =>
                    args(0) match
                        case Int(i) => MayFail.success(Char(IntLattice[I].toChar(i)))
                        case _      => MayFail.failure(OperatorNotApplicable("integer->char", args))
                case SymbolToString =>
                    args(0) match
                        case Symbol(s) => MayFail.success(Str(SymbolLattice[Sym].toString(s)))
                        case _         => MayFail.failure(OperatorNotApplicable("symbol->string", args))
                case StringToSymbol =>
                    args(0) match
                        case Str(s) => MayFail.success(Symbol(StringLattice[S].toSymbol(s)))
                        case _      => MayFail.failure(OperatorNotApplicable("string->symbol", args))
                case ExactToInexact =>
                    args(0) match
                        case Int(n)  => MayFail.success(Real(IntLattice[I].toReal(n)))
                        case Real(n) => MayFail.success(Real(n))
                        case _       => MayFail.failure(OperatorNotApplicable("exact->inexact", args))
                case InexactToExact =>
                    args(0) match
                        case Int(n)  => MayFail.success(Int(n))
                        case Real(n) => MayFail.success(Int(RealLattice[R].toInt[I](n))) /* should introduce fractions */
                        case _       => MayFail.failure(OperatorNotApplicable("inexact->exact", args))
                case CharacterToInteger =>
                    args(0) match
                        case Char(c) => MayFail.success(Int(CharLattice[C].toInt[I](c)))
                        case _       => MayFail.failure(OperatorNotApplicable("char->integer", args))
                case CharacterToString =>
                    args(0) match
                        case Char(c) => MayFail.success(Str(CharLattice[C].toString(c)))
                        case _       => MayFail.failure(OperatorNotApplicable("char->string", args))
                case CharacterDowncase =>
                    args(0) match
                        case Char(c) => MayFail.success(Char(CharLattice[C].downCase(c)))
                        case _       => MayFail.failure(OperatorNotApplicable("char-downcase", args))
                case CharacterUpcase =>
                    args(0) match
                        case Char(c) => MayFail.success(Char(CharLattice[C].upCase(c)))
                        case _       => MayFail.failure(OperatorNotApplicable("char-upcase", args))
                case CharacterIsLower =>
                    args(0) match
                        case Char(c) => MayFail.success(Bool(CharLattice[C].isLower(c)))
                        case _       => MayFail.failure(OperatorNotApplicable("char-lower-case?", args))
                case CharacterIsUpper =>
                    args(0) match
                        case Char(c) => MayFail.success(Bool(CharLattice[C].isUpper(c)))
                        case _       => MayFail.failure(OperatorNotApplicable("char-upper-case?", args))
                case Plus =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].plus(n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].plus(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].plus(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].plus(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("+", args))
                case Minus =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].minus(n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].minus(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].minus(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].minus(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("-", args))
                case Times =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].times(n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].times(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].times(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].times(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("*", args))
                case Quotient =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2)) =>
                            try MayFail.success(Int(IntLattice[I].quotient(n1, n2)))
                            catch
                                case _: ArithmeticException =>
                                    MayFail.failure(OperatorNotApplicable("quotient", args))
                        case _ => MayFail.failure(OperatorNotApplicable("quotient", args))
                case Div =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Real(IntLattice[I].div[R](n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].div(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].div(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].div(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("/", args))
                case Expt =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].expt(n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].expt(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].expt(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].expt(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("expt", args))
                case Modulo =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2)) => MayFail.success(Int(IntLattice[I].modulo(n1, n2)))
                        case _                  => MayFail.failure(OperatorNotApplicable("modulo", args))
                case Remainder =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2)) => MayFail.success(Int(IntLattice[I].remainder(n1, n2)))
                        case _                  => MayFail.failure(OperatorNotApplicable("remainder", args))
                case Lt =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Bool(IntLattice[I].lt(n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Bool(RealLattice[R].lt(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Bool(RealLattice[R].lt(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Bool(RealLattice[R].lt(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("<", args))
                case NumEq =>
                    (args(0), args(1)) match
                        case (Int(n1), Int(n2))   => MayFail.success(Bool(IntLattice[I].eql(n1, n2)))
                        case (Int(n1), Real(n2))  => MayFail.success(Bool(RealLattice[R].eql(IntLattice[I].toReal(n1), n2)))
                        case (Real(n1), Int(n2))  => MayFail.success(Bool(RealLattice[R].eql(n1, IntLattice[I].toReal(n2))))
                        case (Real(n1), Real(n2)) => MayFail.success(Bool(RealLattice[R].eql(n1, n2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("number=", args))
                case StringAppend =>
                    (args(0), args(1)) match
                        case (Str(s1), Str(s2)) => MayFail.success(Str(StringLattice[S].append(s1, s2)))
                        case _                  => MayFail.failure(OperatorNotApplicable("string-append", args))
                case StringRef =>
                    (args(0), args(1)) match
                        case (Str(s), Int(n)) => MayFail.success(Char(StringLattice[S].ref(s, n)))
                        case _                => MayFail.failure(OperatorNotApplicable("string-ref", args))
                case StringSet =>
                    (args(0), args(1), args(2)) match
                        case (Str(s), Int(n), Char(c)) => MayFail.success(Str(StringLattice[S].set(s, n, c)))
                        case _                         => MayFail.failure(OperatorNotApplicable("string-set!", args))
                case StringLt =>
                    (args(0), args(1)) match
                        case (Str(s1), Str(s2)) => MayFail.success(Bool(StringLattice[S].lt(s1, s2)))
                        case _                  => MayFail.failure(OperatorNotApplicable("string<?", args))
                case CharacterEq =>
                    (args(0), args(1)) match
                        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charEq(c1, c2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("char=?", args))
                case CharacterLt =>
                    (args(0), args(1)) match
                        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charLt(c1, c2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("char<?", args))
                case CharacterEqCI =>
                    (args(0), args(1)) match
                        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charEqCI(c1, c2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("char-ci=?", args))
                case CharacterLtCI =>
                    (args(0), args(1)) match
                        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charLtCI(c1, c2)))
                        case _                    => MayFail.failure(OperatorNotApplicable("char-ci<?", args))
                case Substring =>
                    (args(0), args(1), args(2)) match
                        case (Str(s), Int(from), Int(to)) => MayFail.success(Str(StringLattice[S].substring(s, from, to)))
                        case _                            => MayFail.failure(OperatorNotApplicable("substring", args))
                case MakeString =>
                    (args(0), args(1)) match
                        case (Int(length), Char(c)) => MayFail.success(Str(IntLattice[I].makeString(length, c)))
                        case _                      => MayFail.failure(OperatorNotApplicable("make-string", args))

        def car(x: Value): MayFail[L, Error] = x match
            case Cons(car, _) => MayFail.success(car)
            case _            => MayFail.failure(TypeError("expecting cons to access car", x))
        def cdr(x: Value): MayFail[L, Error] = x match
            case Cons(_, cdr) => MayFail.success(cdr)
            case _            => MayFail.failure(TypeError("expecting cons to access cdr", x))
        // This implementation is not suited for use in a concrete machine!
        def vectorRef(vector: Value, index: Value): MayFail[L, Error] = (vector, index) match
            case (Vec(size, content), Int(index)) =>
                val comp = IntLattice[I].lt(index, size)
                val t: L = if BoolLattice[B].isTrue(comp) then
                    val vals = content.view.filterKeys(index2 => BoolLattice[B].isTrue(IntLattice[I].eql(index, index2))).values
                    if vals.isEmpty then schemeLattice.bottom
                    else schemeLattice.join(vals)
                else schemeLattice.bottom
                /* Don't perform bound checks here because we would get too many spurious flows */
                val f: L = schemeLattice.bottom
                MayFail.success(schemeLattice.join(t, f))
            case (_: Vec, _) => MayFail.failure(TypeError("expecting int to access vector", index))
            case _           => MayFail.failure(TypeError("vector-ref: expecting vector", vector))

        // This implementation is not suited for use in a concrete machine!
        def vectorSet(
            vector: Value,
            index: Value,
            newval: L
          ): MayFail[L, Error] =
            (vector, index) match
                case (Vec(size, content), Int(index)) =>
                    val comp = IntLattice[I].lt(index, size)
                    val t: L =
                        if BoolLattice[B].isTrue(comp) then
                            content.find({ case (k, _) => IntLattice[I].subsumes(k, index) }) match
                                case Some((index2, _)) =>
                                    // Case 1: there is an `index2` that already subsumes `index`
                                    // Then we just update the value for `index2`
                                    Element(
                                      Vec(
                                        size,
                                        content + (index2 -> schemeLattice.join(content(index2), newval))
                                      )
                                    )
                                case None =>
                                    val subsumedKeys = content.keySet.filter(k => IntLattice[I].subsumes(index, k))
                                    if subsumedKeys.nonEmpty then
                                        // Case 2: this index subsumes other indices
                                        // In that case, we join all values and removed the subsumed indices
                                        val joinedValues = schemeLattice.join(content.view.filterKeys(subsumedKeys).toMap.values)
                                        val contentWithoutSubsumedKeys = subsumedKeys.foldLeft(content)((acc, k) => acc - k)
                                        Element(Vec(size, contentWithoutSubsumedKeys + (index -> schemeLattice.join(joinedValues, newval))))
                                    else
                                        // Case 3: there is nothing in `content` that we can update, so we add a new key
                                        Element(Vec(size, content + (index -> newval)))
                        else schemeLattice.bottom
                    // We ignore out-of-bounds accesses, mostly because most of them will be spurious.
                    // For example, vector-set! called with Int as first argument would result in
                    // a possible out-of-bound access
                    val f: L = schemeLattice.bottom
                    MayFail.success(schemeLattice.join(t, f))
                case (_: Vec, _) => MayFail.failure(TypeError("expecting int to set vector", index))
                case _           => MayFail.failure(TypeError("vector-set!: expecting vector", vector))

        def vector(size: Value, init: L): MayFail[Value, Error] = size match
            case Int(size) =>
                MayFail.success(if init == IntLattice[I].bottom then {
                    Vec(size, Map[I, L]())
                } else {
                    // Field-sensitive vectors:
                    // Vec(size, Map.from[I, L](IntLattice[I].valuesBetween(IntLattice[I].inject(0), size).map(idx => idx -> init).toList))
                    // Field-insensitive vectors:
                    Vec(size, Map[I, L](IntLattice[I].top -> init))
                })
            case _ => MayFail.failure(TypeError("expected int size when constructing vector", size))

        // Indicates whether a lock is held.
        //def isHeld(lock: Value): MayFail[L, Error] = lock match {
        //  case Lock(tids) => MayFail.success(Element(bool(tids.nonEmpty)))
        //  case _          => MayFail.failure(TypeError("acquire: expected a lock", lock))
        //}

        // Acquire creates a new lock to which the given TID is added.
        def acquire(lock: Value, tid: TID): MayFail[L, Error] = lock match
            case Lock(tids) => MayFail.success(Element(Lock(tids + tid)))
            case _          => MayFail.failure(TypeError("acquire: expected a lock", lock))

        def release(lock: Value, tid: TID): MayFail[L, Error] = lock match
            case Lock(tids) if tids.contains(tid) => MayFail.success(Element(Lock(tids - tid)))
            case Lock(_) => MayFail.failure(InvalidRelease("Cannot release lock since it is not held by the requesting thread.", lock))
            case _       => MayFail.failure(TypeError("release: expected a lock", lock))

        def refs(x: Value): Set[Address] = x match
            case Bool(_) | Char(_) | Int(_) | Real(_) | Str(_) | Symbol(_) | Prim(_) | Void | Nil | Blames(_) => Set.empty
            case InputPort(id)                                                                                => schemeLattice.refs(id)
            case OutputPort(id)                                                                               => schemeLattice.refs(id)
            case Thread(_) | Lock(_) | Kont(_) => ??? // TODO: don't know enough about these types to compute refs
            case Pointer(ptrs)                 => ptrs.toSet[Address]
            case Cons(car, cdr)                => sunion(schemeLattice.refs(car), schemeLattice.refs(cdr))
            case Vec(_, els)                   => els.flatMap(elm => schemeLattice.refs(elm._2)).toSet
            case Clo(cls)                      => cls.flatMap(clo => clo._2.addrs)
            case Arrs(arrs)  => arrs.flatMap(arr => schemeLattice.refs(schemeLattice.grd(arr.contract)) ++ schemeLattice.refs(arr.e))
            case Grds(grds)  => grds.flatMap(grd => grd.domain.flatMap(schemeLattice.refs(_)) ++ schemeLattice.refs(grd.rangeMaker))
            case Flats(flts) => flts.flatMap(flt => schemeLattice.refs(flt.contract))

        // TODO: this should be the eql method instead?
        def eq(x: Value, y: Value)(comparePtr: MaybeEq[A]): Value = (x, y) match
            case (Bool(b1), Bool(b2))     => Bool(BoolLattice[B].eql(b1, b2))
            case (Int(n1), Int(n2))       => Bool(IntLattice[I].eql(n1, n2))
            case (Real(n1), Real(n2))     => Bool(RealLattice[R].eql(n1, n2))
            case (Char(c1), Char(c2))     => Bool(CharLattice[C].eql(c1, c2))
            case (Symbol(s1), Symbol(s2)) => Bool(SymbolLattice[Sym].eql(s1, s2))
            case (Nil, Nil)               => True
            case (Prim(p1), Prim(p2))     => if p1.intersect(p2).isEmpty then Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
            // TODO: eq of closures could be improved, but is not really permitted by R5RS anyway ...
            case (Clo(c1), Clo(c2))       => if c1.intersect(c2).isEmpty then Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
            case (Blames(b1), Blames(b2)) => if b1.intersect(b2).isEmpty then Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
            // TODO: implement eq for contract values and opq values
            case (_: Cons, _: Cons) => throw new Exception("should not happen")
            case (_: Vec, _: Vec)   => throw new Exception("should not happen")
            case (_: Str, _: Str)   => throw new Exception("should not happen")
            case (Pointer(p1), Pointer(p2)) =>
                Bool(p1.foldLeft(BoolLattice[B].bottom) { (acc1, ptr1) =>
                    p2.foldLeft(acc1) { (acc2, ptr2) =>
                        BoolLattice[B].join(acc2, comparePtr(ptr1, ptr2))
                    }
                })
            // We can't know for sure that equal addresses are eq (in the abstract). This implementation is not suited for use in a concrete machine!
            case (Thread(t1), Thread(t2)) => if t1.intersect(t2).isEmpty then Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
            case _                        => False

    type L = HMap
    //case class Elements(vs: List[Value]) extends SmartHash:
    //    override def toString: String =
    //        if vs.isEmpty then "⊥"
    //        else if vs.tail.isEmpty then vs.head.toString
    //        else vs.map(_.toString).sorted.mkString("{", ",", "}")
    //    def foldMapL[X](f: Value => X)(implicit monoid: Monoid[X]): X =
    //        vs.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))
    //

    // This object simulates the usage of the now legacy "Elements" case class.
    object Elements:
        /** Convience method to be able to construct values of the modular scheme lattice in the old way. */
        @deprecated("Elements is being replaced by HMap, use HMap instead of Elements", "2022-08-19")
        def apply(vs: Iterable[Value]): L =
            // NOTE: the usage of asInstanceOf here is slightly unsafe since Value.tpy.Wrap might not correspond to the actual type of v
            // we trust the programmers to not make this mistake, but it cannot be enforcement at compile-time.
            // Removal of these wrappers might (partially) solve the problem.
            vs.foldLeft(HMap.empty)((hmap, v) => hmap.wrapInsert(v.tpy, v.asInstanceOf[v.tpy.Wrap]))

        @deprecated("Elements is being replaced by HMap, use HMap instead of Elements", "2022-08-19")
        def unapply(v: L): Option[(List[Value])] =
            Some(v.elements[Value])

        extension (v: L) def vs: List[Value] = v.elements[Value]

    object Element extends Serializable:
        def apply(v: Value): L =
            // NOTE: the usage of asInstanceOf here is slightly unsafe since Value.tpy.Wrap might not correspond to the actual type of v
            // we trust the programmers to not make this mistake, but it cannot be enforcement at compile-time.
            // Removal of these wrappers might (partially) solve the problem.
            HMap.wrapInserted(v.tpy, v.asInstanceOf[v.tpy.Wrap])

    import MonoidInstances._
    implicit val lMonoid: Monoid[L] = new Monoid[L] {
        def append(x: L, y: => L): L =
            Lattice[L].join(x, y)
        def zero: L = Lattice[L].bottom
    }
    implicit val lMFMonoid: Monoid[MayFail[L, Error]] = MonoidInstances.mayFail[L]

    // TODO: field SchemeLattice only exists for backwards compatibility reasons, but for the sake of simplification should also be removed, and the methods be included within the class itself
    val schemeLattice: SchemeLattice[L, A] = new SchemeLattice[L, A] { lat =>
        def show(x: L): String = x.toString /* TODO[easy]: implement better */
        def isTrue(x: L): Boolean = x.elements[Value].foldMap(Value.isTrue(_))(boolOrMonoid)
        def isFalse(x: L): Boolean = x.elements[Value].foldMap(Value.isFalse(_))(boolOrMonoid)
        def isBoolean(x: L): Boolean = x.elements[Value].foldMap(Value.isBoolean(_))(boolOrMonoid)
        def retractBool(x: L): L = ??? // TODO: implement (does anybody uses this)?
        def isOpq(x: L): Boolean = x.elements[Value].foldMap(Value.isOpq(_))(boolOrMonoid)

        def op(op: SchemeOp)(args: List[L]): MayFail[L, Error] =
            def fold(argsToProcess: List[L], argsvRev: List[Value]): MayFail[L, Error] = argsToProcess match
                case arg :: args =>
                    arg.elements[Value].foldMap(argv => fold(args, argv :: argsvRev))
                case List() =>
                    val argsv = argsvRev.reverse
                    op match
                        case SchemeOp.Car => Value.car(argsv(0))
                        case SchemeOp.Cdr => Value.cdr(argsv(0))
                        case SchemeOp.VectorRef =>
                            Value.vectorRef(argsv(0), argsv(1))
                        case SchemeOp.VectorSet =>
                            Value.vectorSet(argsv(0), argsv(1), args(2))
                        case _ => Value.op(op)(argsv).map(x => Element(x))
            op.checkArity(args)
            op match
                case SchemeOp.MakeInputPort =>
                    if schemeLattice.isBottom(args(0)) then MayFail.success(schemeLattice.bottom)
                    else MayFail.success(Element(InputPortT.wrap(args(0))))
                case SchemeOp.MakeOutputPort =>
                    if schemeLattice.isBottom(args(0)) then MayFail.success(schemeLattice.bottom)
                    else MayFail.success(Element(OutputPortT.wrap(args(0))))
                case SchemeOp.MakeVector =>
                    /* Treated as a special case because args(1) can be bottom (this would be a valid use of MakeVector) */
                    args(0).elements[Value].foldMap(arg0 => Value.vector(arg0, args(1)).map(v => Element(v)))
                case _ => fold(args, List())

        def join(x: L, y: => L): L = Lattice[L].join(x, y)
        def subsumes(x: L, y: => L): Boolean = Lattice[L].subsumes(x, y)
        def top: L = throw LatticeTopUndefined

        def getClosures(x: L): Set[Closure] = x.get(CloT)
        def getContinuations(x: L): Set[lat.K] = x.get(KontT)
        def getPrimitives(x: L): Set[String] = x.get(PrimT)
        def getPointerAddresses(x: L): Set[A] = x.get(PointerT)
        def getBlames(x: L): Set[Blame] = x.get(BlameT)
        def getGrds(x: L): Set[Grd[L]] = x.get(GrdT)
        def getArrs(x: L): Set[Arr[L]] = x.get(ArrT)
        def getFlats(x: L): Set[Flat[L]] = x.get(FlatT)
        def getStructs(x: L): Set[Struct[L]] = x.get(StructT)
        def getGetterSetter(x: L): Set[StructSetterGetter] = x.get(StructSetterGetterT)
        def getStructConstructor(x: L): Set[StructConstructor] = x.get(StructConstructorT)
        def getStructPredicates(x: L): Set[StructPredicate] = x.get(StructPredicateT)
        def getThreads(x: L): Set[TID] = x.get(ThreadT)

        def acquire(lock: L, tid: TID): MayFail[L, Error] =
            lock.elements[Value].foldMap(l => Value.acquire(l, tid))
        def release(lock: L, tid: TID): MayFail[L, Error] =
            lock.elements[Value].foldMap(l => Value.release(l, tid))

        def bottom: L = Lattice[L].bottom

        def number(x: BigInt): L = HMap.injected(IntT, x)

        def numTop: L = HMap.inserted(IntT, IntLattice[I].top)
        def charTop: L = HMap.inserted(CharT, CharLattice[C].top)
        def realTop: L = HMap.inserted(RealT, RealLattice[R].top)
        def stringTop: L = HMap.inserted(StrT, StringLattice[S].top)
        def symbolTop: L = HMap.inserted(SymbolT, SymbolLattice[Sym].top)
        def real(x: Double): L = HMap.injected(RealT, x)
        def string(x: String): L = HMap.injected(StrT, x)
        def char(x: scala.Char): L = HMap.injected(CharT, x)
        def bool(x: Boolean): L = HMap.injected(BoolT, x)
        def primitive(x: String): L = HMap.injected(PrimT, x)
        def closure(x: Closure): L = HMap.injected(CloT, x)
        def cont(x: lat.K): L = HMap.injected(KontT, x)
        def symbol(x: String): L = HMap.injected(SymbolT, x)
        def cons(car: L, cdr: L): L = HMap.wrapInserted(ConsT, Cons(car, cdr))
        def pointer(a: A): L = HMap.injected(PointerT, a)
        def thread(tid: TID): L = HMap.injected(ThreadT, tid)
        def lock(threads: Set[TID]): L = HMap.inserted(LockT, threads)
        def blame(blame: Blame): L = HMap.injected(BlameT, blame)
        def grd(grd: Grd[L]): L = HMap.injected(GrdT, grd)
        def arr(arr: Arr[L]): L = HMap.injected(ArrT, arr)
        def flat(flt: Flat[L]): L = HMap.injected(FlatT, flt)
        def opq(opq: Opq): L = HMap.injected(OpqT, opq)

        def rmods(mod: HMap): Set[RMod[HMap]] = mod.get(RModT)
        def rmod(mod: RMod[HMap]): HMap = HMap.injected(RModT, mod)

        def struct(struct: Struct[L]): L = HMap.injected(StructT, struct)
        def structPredicate(struct: StructPredicate): L = HMap.injected(StructPredicateT, struct)
        def structSetterGetter(setterGetter: StructSetterGetter): L =
            HMap.injected(StructSetterGetterT, setterGetter)
        def structConstructor(constructor: StructConstructor): L =
            HMap.injected(StructConstructorT, constructor)
        def nil: L = HMap.wrapInserted(NilT, Nil)
        def void: L = HMap.wrapInserted(VoidT, Void)
        def eql[B2: BoolLattice](x: L, y: L): B2 =
            x.elements[Value].foldMap[B2] {
                case Symbol(s1) =>
                    y.elements.foldMap[B2] {
                        case Symbol(s2) => SymbolLattice[Sym].eql[B2](s1, s2)
                        case _          => BoolLattice[B2].top
                    }
                case _ => BoolLattice[B2].top
            }
        def refs(x: L): Set[Address] = x.elements[Value].foldMap(x => Value.refs(x))(setMonoid) // TODO: put in HMap lattice
        def eq(xs: L, ys: L)(comparePtr: MaybeEq[A]): L = // TODO: put in HMap lattice
            xs.elements[Value].foldMap { x =>
                ys.elements[Value].foldMap { y =>
                    Element(Value.eq(x, y)(comparePtr))
                }
            }
    }

    override def eq(x: L, y: L)(comparePtr: MaybeEq[A]): L = schemeLattice.eq(x, y)(comparePtr)

    private def emptyEnv[A <: Address] = Environment[A](Iterable.empty)

    def convertV[BaseAddr <: Address](
        baseDomain: ModularSchemeLattice[A, S, B, I, R, C, Sym]
      )(
        aux: ModularSchemeConvertor[BaseAddr, L]
      )(
        value: Value
      ): baseDomain.Value =
        value match
            case Nil        => baseDomain.Nil
            case Bool(b)    => baseDomain.Bool(b)
            case Int(i)     => baseDomain.Int(i)
            case Real(r)    => baseDomain.Real(r)
            case Char(c)    => baseDomain.Char(c)
            case Str(s)     => baseDomain.Str(s)
            case Symbol(s)  => baseDomain.Symbol(s)
            case Prim(ps)   => baseDomain.Prim(ps)
            case Clo(cs)    => baseDomain.Clo(cs.map(c => (c._1, emptyEnv)))
            case Cons(a, d) => baseDomain.Cons(aux.convertValue(baseDomain)(a), aux.convertValue(baseDomain)(d))
            //TODO:case Pointer(ps)  => baseDomain.Pointer(ps.map(aux.convertAddr))
            case Vec(s, e)    => baseDomain.Vec(s, e.view.mapValues(aux.convertValue(baseDomain)).toMap)
            case Void         => baseDomain.Void
            case Lock(tids)   => baseDomain.Lock(tids)
            case Thread(tids) => baseDomain.Thread(tids)
            case v            => throw new Exception(s"Unsupported value type for conversion: ${v.ord}.")

    // Make this an instance of SchemeLattice
    export L.lattice.{eq => _, *}

    object L:
        implicit val lattice: SchemeLattice[L, A] = schemeLattice
