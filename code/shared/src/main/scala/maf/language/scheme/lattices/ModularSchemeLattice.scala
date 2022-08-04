package maf.language.scheme.lattices

import maf.core._
import maf.language.CScheme.TID
import maf.language.ContractScheme.ContractValues._
import maf.language.AScheme.ASchemeValues.{Actor, Behavior}
import maf.language.scheme.primitives.SchemePrimitive
import maf.lattice.interfaces._
import maf.util.datastructures.SmartUnion._
import maf.util._
import smtlib.theories.Core.False
import maf.language.AScheme.ASchemeValues.Future
import maf.language.scheme.SchemeLambdaExp
import maf.lattice.*

/**
 * Defines a Scheme lattice based on other lattices. Example usage: val address = NameAddress val lattice = new ModularSchemeLattice[SchemeExp,
 * address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym] Now `lattice.L` is a SchemeLattice, of which the implicit for the typeclass is
 * available in the current scope.
 */
/** TODO[medium]: use Show and ShowStore here */
class ModularSchemeLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends Serializable:

    trait ModularSchemeConvertor[BaseAddr <: Address, V]:
        def convertAddr(addr: Address): BaseAddr
        def convertValue(baseDomain: ModularSchemeLattice[A, S, B, I, R, C, Sym])(v: V): baseDomain.L

    // TODO: make this a type parameter for type safety!
    type K = Any

    type P = SchemePrimitive[L, A]

    implicit def mfAddrMonoid[X]: Monoid[MayFail[Set[X], Error]] = MonoidImplicits.mayFail[Set[X]](MonoidImplicits.setMonoid[X])

    given showImpl[T]: Show[T] with
        def show(v: T): String = v.toString

    /** All values are 'wrapped' here into a single value, for this we need type markers based on the type parameters of the ModularSchemeLattice */
    case object StrT extends AType[S, String]
    case object BoolT extends AType[B, Boolean]
    case object IntT extends AType[I, BigInt]
    case object RealT extends AType[R, Double]
    case object CharT extends AType[C, scala.Char]
    case object SymT extends AType[Sym, String]
    case object PrimT extends ATypeSet[String]
    case object CloT extends ATypeSet[schemeLattice.Closure]
    case object NilT extends ATypeSingleton
    case object PointerT extends ATypeSet[A]
    // TODO: make a cons lattice
    // TODO: make a vector lattice
    case object KontT extends ATypeSet[K]
    case object ThreadT extends ATypeSet[TID]
    case object LockT extends ATypeSet[TID]
    case object VoidT extends ATypeSingleton
    // TODO: add input port + output port
    case object BlameT extends ATypeSet[Blame]
    case object GrdT extends ATypeSet[Grd[L]]
    case object OpqT extends ATypeSet[Opq]
    case object ArrT extends ATypeSet[Arr[L]]
    case object FlatT extends ATypeSet[Flat[L]]
    //
    case object StructT extends ATypeSet[Struct[L]]
    case object StructSetterGetterT extends ATypeSet[StructSetterGetter]
    case object StructConstructorT extends ATypeSet[StructConstructor]
    case object StructPredicateT extends ATypeSet[StructPredicate]
    //
    case object ActorT extends ATypeSet[Actor]
    case object BehaviorT extends ATypeSet[Behavior]
    case object FutureT extends ATypeSet[Future]

    /**
     * We first implement all possible operations on single values, that can be only joined when compatible. This therefore is not a lattice but will
     * be used to build the set lattice
     */
    sealed trait Value extends SmartHash:
        def ord: scala.Int
        def typeName: String // Can be used to print information on values.
    case class Str(s: S) extends Value:
        def ord = 0
        def typeName = "STRG"
        override def toString: String = StringLattice[S].show(s)
    case class Bool(b: B) extends Value:
        def ord = 1
        def typeName = "BOOL"
        override def toString: String = BoolLattice[B].show(b)
    case class Int(i: I) extends Value:
        def ord = 2
        def typeName = "INTE"
        override def toString: String = IntLattice[I].show(i)
    case class Real(r: R) extends Value:
        def ord = 3
        def typeName = "REAL"
        override def toString: String = RealLattice[R].show(r)
    case class Char(c: C) extends Value:
        def ord = 4
        def typeName = "CHAR"
        override def toString: String = CharLattice[C].show(c)
    case class Symbol(s: Sym) extends Value:
        def ord = 5
        def typeName = "SYMB"
        override def toString: String = SymbolLattice[Sym].show(s)
    case class Prim(prims: Set[String]) extends Value:
        def ord = 6
        def typeName = "PRIM"
        override def toString: String = prims.mkString("Primitive{", ", ", "}")
    // TODO: define `type Closure = (SchemeLambdaExp, Env)` (maybe using a case class)
    case class Clo(closures: Set[schemeLattice.Closure]) extends Value:
        def ord = 7
        def typeName = "CLOS"
        override def toString: String =
            closures
                .map(_._1.lambdaName)
                .mkString("Closures{", ", ", "}")
    case object Nil extends Value:
        def ord = 8
        def typeName = "NULL"
        override def toString: String = "()"
    case class Pointer(ptrs: Set[A]) extends Value:
        def ord = 9
        def typeName = "PNTR"
        override def toString: String = ptrs.mkString("Pointers{", ", ", "}")
    case class Cons(car: L, cdr: L) extends Value:
        def ord = 10
        def typeName = "CONS"
        override def toString: String = s"($car . $cdr)"
    case class Vec(size: I, elements: Map[I, L]) extends Value:
        def ord = 11
        def typeName = "VECT"
        override def toString: String =
            val els = elements.toList
                .map({ case (k, v) =>
                    s"$k: $v"
                })
                .mkString(", ")
            s"Vector(size: $size, elems: {$els})"
    case class Kont(k: Set[K]) extends Value:
        def ord = 12
        def typeName = "KONT"
        override def toString: String = "<continuation>"
    case class Thread(threads: Set[TID]) extends Value:
        def ord = 13
        def typeName = "THRD"
        override def toString: String = threads.mkString("Thread{", ", ", "}")
    // Could also store (a) Thread(s) here, but this seems to be simpler.
    // An empty set indicates the lock is not held, but a non-empty set may also indicate this... (due to the monotonicity of the analysis, threads will only increase in size).
    // This should correspond to the formalisation of ModConc and \lambda_\tau.
    case class Lock(threads: Set[TID]) extends Value:
        def ord = 14
        def typeName = "LOCK"
        override def toString: String = threads.mkString("Lock{", ", ", "}")

    case object Void extends Value:
        def ord = 15
        def typeName = "VOID"
        override def toString: String = "<void>"
    case class InputPort(id: Value) extends Value:
        def ord = 16
        def typeName = "IPRT"
        override def toString: String = s"InputPort{$id}"
    case class OutputPort(id: Value) extends Value:
        def ord = 17
        def typeName = "OPRT"
        override def toString: String = s"OutputPort{$id}"

    case class Blames(blames: Set[Blame]) extends Value:
        def ord = 18
        def typeName = "BLAME"
        override def toString: String = s"<blame: ${blames.map(_.toString).mkString(",")}>"

    case class Grds(grds: Set[Grd[L]]) extends Value:
        def ord = 19
        def typeName = "GRD"
        override def toString: String = s"<grd: ${grds.map(_.toString).mkString(",")}>"

    case class Arrs(arrs: Set[Arr[L]]) extends Value:
        def ord = 20
        def typeName = "ARR"
        override def toString: String = s"<arr: ${arrs.map(_.toString).mkString(",")}>"

    case class Flats(flats: Set[Flat[L]]) extends Value:
        def ord = 21
        def typeName = "FLT"
        override def toString: String = s"<flat: ${flats.map(_.toString).mkString(",")}>"

    case class Opqs(opqs: Set[Opq]) extends Value:
        def ord = 22
        def typeName = "OPQ"
        override def toString: String = s"<opq>"

    case class Structs(structs: Set[Struct[L]]) extends Value:
        def ord = 23
        def typeName = "STRUCT"
        override def toString: String = s"<struct: ${structs.map(_.tag).mkString(",")}>"

    case class StructSetterGetters(getterSetters: Set[StructSetterGetter]) extends Value:
        def ord = 24
        def typeName = "STRUCTGETTERSETTER"
        override def toString: String = "<struct-getter-setter>"

    case class StructConstructors(constructors: Set[StructConstructor]) extends Value:
        def ord = 25
        def typeName = "STRUCTCONSTRUCTOR"
        override def toString: String = "<struct-constructor>"

    case class StructPredicates(predicates: Set[StructPredicate]) extends Value:
        def ord = 26
        def typeName = "STRUCTPREDICATE"
        override def toString: String = "<struct-predicate>"

    case class Actors(actors: Set[Actor]) extends Value:
        def ord = 27
        def typeName = "ACTOR"
        override def toString: String = s"<actor {${actors.mkString(",")}}>"
    case class Behaviors(behs: Set[Behavior]) extends Value:
        def ord = 28
        def typeName = "BEH"
        override def toString: String = s"<behavior>"
    case class Futures(futures: Set[Future]) extends Value:
        def ord = 29
        def typeName = "FUT"
        override def toString: String = s"<future>"

    /** The injected true value */
    val True: Bool = Bool(BoolLattice[B].inject(true))

    /** The injected false value */
    val False: Bool = Bool(BoolLattice[B].inject(false))

    object Value:
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
                    MayFail.success(bool(isTrue(args(0))))
                case IsFalse =>
                    MayFail.success(bool(isFalse(args(0))))
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
                case MakeInputPort  => MayFail.success(InputPort(args(0)))
                case MakeOutputPort => MayFail.success(OutputPort(args(0)))
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
            case InputPort(id)                                                                                => refs(id)
            case OutputPort(id)                                                                               => refs(id)
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

    implicit val lMFMonoid: Monoid[MayFail[L, Error]] = MonoidInstances.mayFail[L]

    val schemeLattice: SchemeLattice[L, A] = new SchemeLattice[L, A] { lat =>
        export HMap.lat.*
        def isTrue(x: L): Boolean = x.foldMapL(Value.isTrue(_))(boolOrMonoid)
        def isFalse(x: L): Boolean = x.foldMapL(Value.isFalse(_))(boolOrMonoid)
        def isBoolean(x: L): Boolean = x.foldMapL(Value.isBoolean(_))(boolOrMonoid)
        def retractBool(x: L): L = Elements(x.vs.filter {
            case Bool(_) => false
            case _       => true
        })

        def isOpq(x: L): Boolean = x.foldMapL(Value.isOpq(_))(boolOrMonoid)

        def op(op: SchemeOp)(args: List[L]): MayFail[L, Error] =
            def fold(argsToProcess: List[L], argsvRev: List[Value]): MayFail[L, Error] = argsToProcess match
                case arg :: args =>
                    arg.foldMapL(argv => fold(args, argv :: argsvRev))
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
                case SchemeOp.MakeVector =>
                    /* Treated as a special case because args(1) can be bottom (this would be a valid use of MakeVector) */
                    args(0).foldMapL(arg0 => Value.vector(arg0, args(1)).map(v => Element(v)))
                case _ => fold(args, List())

        def top: L = throw LatticeTopUndefined

        def getClosures(x: L): Set[Closure] = x.get(CloT).getOrElse(Set())
        def getContinuations(x: L): Set[lat.K] = x.get(KontT).getOrElse(Set())
        def getPrimitives(x: L): Set[String] = x.get(PrimT).getOrElse(Set())
        def getPointerAddresses(x: L): Set[A] = x.get(PointerT).getOrElse(Set())
        def getBlames(x: L): Set[Blame] = x.get(BlameT).getOrElse(Set())
        def getGrds(x: L): Set[Grd[L]] = x.get(GrdT).getOrElse(Set())
        def getArrs(x: L): Set[Arr[L]] = x.get(ArrT).getOrElse(Set())
        def getFlats(x: L): Set[Flat[L]] = x.get(FlatT).getOrElse(Set())
        def getStructs(x: L): Set[Struct[L]] = x.get(StructT).getOrElse(Set())
        def getGetterSetter(x: L): Set[StructSetterGetter] = x.get(StructSetterGetterT).getOrElse(Set())
        def getStructConstructor(x: L): Set[StructConstructor] = x.get(StructConstructorT).getOrElse(Set())
        def getStructPredicates(x: L): Set[StructPredicate] = x.get(StructPredicateT).getOrElse(Set())
        def getThreads(x: L): Set[TID] = x.get(ThreadT).getOrElse(Set())
        def getActors(x: L): Set[Actor] = x.get(ActorT).getOrElse(Set())
        def getBehs(x: L): Set[Behavior] = x.get(BehaviorT).getOrElse(Set())
        def getFutures(x: L): Set[Future] = x.get(FutureT).getOrElse(Set())

        def acquire(lock: L, tid: TID): MayFail[L, Error] =
            lock.foldMapL(l => Value.acquire(l, tid))
        def release(lock: L, tid: TID): MayFail[L, Error] =
            lock.foldMapL(l => Value.release(l, tid))

        def numTop: L = HMap.top(IntT)
        def charTop: L = HMap.top(CharT)
        def realTop: L = HMap.top(RealT)
        def stringTop: L = HMap.top(StrT)
        def symbolTop: L = HMap.top(SymT)
        def number(x: BigInt): L = HMap.inject(IntT, x)
        def real(x: Double): L = HMap.inject(RealT, x)
        def string(x: String): L = HMap.inject(StrT, x)
        def char(x: scala.Char): L = HMap.inject(CharT, x)
        def bool(x: Boolean): L = HMap.inject(BoolT, x)
        def primitive(x: String): L = HMap.inject(PrimT, x)
        def closure(x: Closure): L = HMap.inject(CloT, x)
        def cont(x: lat.K): L = HMap.inject(KontT, x)
        def symbol(x: String): L = HMap.inject(SymT, x)
        def cons(car: L, cdr: L): L = Element(Value.cons(car, cdr))
        def pointer(a: A): L = HMap.inject(PointerT, a)
        def thread(tid: TID): L = HMap.inject(ThreadT, tid)
        def lock(threads: Set[TID]): L = HMap.inserted(LockT, threads)
        def actor(actor: Actor): L = HMap.inject(ActorT, actor)
        def beh(behavior: Behavior): L = HMap.inject(BehaviorT, behavior)
        def future(fut: Future): L = HMap.inject(FutureT, fut)
        def blame(blame: Blame): L = HMap.inject(BlameT, blame)
        def grd(grd: Grd[L]): L = HMap.inject(GrdT, grd)
        def arr(arr: Arr[L]): L = HMap.inject(ArrT, arr)
        def flat(flt: Flat[L]): L = HMap.inject(FlatT, flt)
        def opq(opq: Opq): L = HMap.inject(OpqT, opq)
        def struct(struct: Struct[L]): L = HMap.inject(StructT, struct)
        def structPredicate(struct: StructPredicate): L = HMap.inject(StructPredicateT, struct)
        def structSetterGetter(setterGetter: StructSetterGetter): L =
            HMap.inject(StructSetterGetterT, setterGetter)
        def structConstructor(constructor: StructConstructor): L =
            HMap.inject(StructConstructorT, constructor)
        def nil: L = ???
        def void: L = ???
        def refs(x: L): Set[Address] = ??? // x.foldMapL(x => Value.refs(x))(setMonoid)
    }

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

    object L:
        implicit val lattice: SchemeLattice[L, A] = schemeLattice
