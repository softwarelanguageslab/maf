package maf.language.scheme.lattices

import maf.core._
import maf.language.CScheme.TID
import maf.language.ContractScheme.ContractValues._
import maf.language.scheme.primitives.SchemePrimitive
import maf.lattice.interfaces._
import maf.util.datastructures.SmartUnion._
import maf.util._

/**
 * Defines a Scheme lattice based on other lattices. Example usage: val address = NameAddress val lattice = new ModularSchemeLattice[SchemeExp,
 * address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym] Now `lattice.L` is a SchemeLattice, of which the implicit for the typeclass is
 * available in the current scope.
 */
/** TODO[medium]: use Show and ShowStore here */
class ModularSchemeLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends Serializable:

    // TODO: make this a type parameter for type safety!
    type K = Any

    type P = SchemePrimitive[L, A]

    implicit def mfAddrMonoid[X]: Monoid[MayFail[Set[X], Error]] = MonoidImplicits.mayFail[Set[X]](MonoidImplicits.setMonoid[X])

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

    /** The injected true value */
    val True: Bool = Bool(BoolLattice[B].inject(true))

    /** The injected false value */
    val False: Bool = Bool(BoolLattice[B].inject(false))

    object Value:

        /** Tries to join (throws an exception for incompatible types) */
        def join(x: Value, y: => Value): Value = (x, y) match
            case (Nil, Nil)                           => Nil
            case (Str(s1), Str(s2))                   => Str(StringLattice[S].join(s1, s2))
            case (Bool(b1), Bool(b2))                 => Bool(BoolLattice[B].join(b1, b2))
            case (Int(i1), Int(i2))                   => Int(IntLattice[I].join(i1, i2))
            case (Real(f1), Real(f2))                 => Real(RealLattice[R].join(f1, f2))
            case (Char(c1), Char(c2))                 => Char(CharLattice[C].join(c1, c2))
            case (Symbol(s1), Symbol(s2))             => Symbol(SymbolLattice[Sym].join(s1, s2))
            case (Prim(p1), Prim(p2))                 => Prim(sunion(p1, p2))
            case (Clo(c1), Clo(c2))                   => Clo(sunion(c1, c2))
            case (Pointer(a1), Pointer(a2))           => Pointer(sunion(a1, a2))
            case (Cons(a1, d1), Cons(a2, d2))         => Cons(schemeLattice.join(a1, a2), schemeLattice.join(d1, d2))
            case (Vec(size1, els1), Vec(size2, els2)) =>
              // First, joins the size
              val vSizeInitJoined = Vec(IntLattice[I].join(size1, size2), Map.empty)
              // Then, joins elements by adding (with vector-set!) all elements of els1 and then els2 inside the new vector
              val vWithEls1Joined = els1.foldLeft(vSizeInitJoined)({ case (acc, (k, v)) =>
                vectorSet(acc, Int(k), v).getOrElse(schemeLattice.bottom) match {
                  case Elements(vs) => vs.head.asInstanceOf[Vec] // Should really be improved, this is ugly
                }
              })
              val vWithEls2Joined = els2.foldLeft(vWithEls1Joined)({ case (acc, (k, v)) =>
                vectorSet(acc, Int(k), v).getOrElse(schemeLattice.bottom) match {
                  case Elements(vs) => vs.head.asInstanceOf[Vec] // Should really be improved, this is ugly
                }
              })
              vWithEls2Joined
            case (Kont(k1), Kont(k2))                               => Kont(sunion(k1, k2))
            case (Thread(t1), Thread(t2))                           => Thread(sunion(t1, t2))
            case (Lock(l1), Lock(l2))                               => Lock(sunion(l1, l2))
            case (Void, Void)                                       => Void
            case (InputPort(l1), InputPort(l2))                     => InputPort(join(l1, l2))
            case (OutputPort(l1), OutputPort(l2))                   => OutputPort(join(l1, l2))
            case (Blames(l1), Blames(l2))                           => Blames(sunion(l1, l2))
            case (Arrs(l1), Arrs(l2))                               => Arrs(sunion(l1, l2))
            case (Grds(l1), Grds(l2))                               => Grds(sunion(l1, l2))
            case (Flats(l1), Flats(l2))                             => Flats(sunion(l1, l2))
            case (Opqs(o1), Opqs(o2))                               => Opqs(sunion(o1, o2))
            case (Structs(o1), Structs(o2))                         => Structs(sunion(o1, o2))
            case (StructSetterGetters(o1), StructSetterGetters(o2)) => StructSetterGetters(o1 ++ o2)
            case (StructConstructors(o1), StructConstructors(o2))   => StructConstructors(o1 ++ o2)
            case (StructPredicates(o1), StructPredicates(o2))       => StructPredicates(o1 ++ o2)
            case _                                                  => throw new Exception(s"Illegal join of $x and $y")

        def subsumes(x: Value, y: => Value): Boolean =
          if x == y then true
          else
              (x, y) match
                  case (Str(s1), Str(s2))           => StringLattice[S].subsumes(s1, s2)
                  case (Bool(b1), Bool(b2))         => BoolLattice[B].subsumes(b1, b2)
                  case (Int(i1), Int(i2))           => IntLattice[I].subsumes(i1, i2)
                  case (Real(f1), Real(f2))         => RealLattice[R].subsumes(f1, f2)
                  case (Char(c1), Char(c2))         => CharLattice[C].subsumes(c1, c2)
                  case (Symbol(s1), Symbol(s2))     => SymbolLattice[Sym].subsumes(s1, s2)
                  case (Clo(c1), Clo(c2))           => c2.subsetOf(c1)
                  case (Prim(p1), Prim(p2))         => p2.subsetOf(p1)
                  case (Pointer(a1), Pointer(a2))   => a2.subsetOf(a1)
                  case (Kont(k1), Kont(k2))         => k2.subsetOf(k1)
                  case (Cons(a1, d1), Cons(a2, d2)) => schemeLattice.subsumes(a1, a2) && schemeLattice.subsumes(d1, d2)
                  case (Vec(siz1, els1), Vec(siz2, els2)) =>
                    IntLattice[I].subsumes(siz1, siz2) &&
                      els2.forall { case (idx2, vlu2) =>
                        els1.exists { case (idx1, vlu1) =>
                          IntLattice[I].subsumes(idx1, idx2) && schemeLattice.subsumes(vlu1, vlu2)
                        }
                      }
                  case (Thread(t1), Thread(t2))                           => t2.subsetOf(t1)
                  case (Lock(l1), Lock(l2))                               => l2.subsetOf(l1)
                  case (InputPort(l1), InputPort(l2))                     => subsumes(l1, l2)
                  case (OutputPort(l1), OutputPort(l2))                   => subsumes(l1, l2)
                  case (Blames(l1), Blames(l2))                           => l2.subsetOf(l1)
                  case (Arrs(l1), Arrs(l2))                               => l2.subsetOf(l1)
                  case (Grds(l1), Grds(l2))                               => l2.subsetOf(l1)
                  case (Flats(l1), Flats(l2))                             => l2.subsetOf(l1)
                  case (Structs(l1), Structs(l2))                         => l2.subsetOf(l1)
                  case (StructSetterGetters(l1), StructSetterGetters(l2)) => l2.subsetOf(l1)
                  case (StructConstructors(l1), StructConstructors(l2))   => l2.subsetOf(l1)
                  case (StructPredicates(l1), StructPredicates(l2))       => l2.subsetOf(l1)
                  case _                                                  => false

        def isTrue(x: Value): Boolean = x match
            case Bool(b) => BoolLattice[B].isTrue(b)
            case _       => true
        def isFalse(x: Value): Boolean = x match
            case Bool(b) => BoolLattice[B].isFalse(b)
            case Opqs(_) => true
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

        def number(x: BigInt): Value = Int(IntLattice[I].inject(x))

        def real(x: Double): Value = Real(RealLattice[R].inject(x))
        def string(x: String): Value = Str(StringLattice[S].inject(x))
        def bool(x: Boolean): Value = Bool(BoolLattice[B].inject(x))
        def char(x: scala.Char): Value = Char(CharLattice[C].inject(x))
        def primitive(x: String): Value = Prim(Set(x))
        def closure(x: schemeLattice.Closure): Value = Clo(Set(x))
        def cont(k: K): Value = Kont(Set(k))
        def symbol(x: String): Value = Symbol(SymbolLattice[Sym].inject(x))
        def nil: Value = Nil
        def cons(car: L, cdr: L): Value = Cons(car, cdr)
        def pointer(a: A): Value = Pointer(Set(a))
        def thread(tid: TID): Value = Thread(Set(tid))
        def lock(threads: Set[TID]): Value = Lock(threads)
        def void: Value = Void
        def blame(blame: Blame): Value = Blames(Set(blame))
        def grd(grd: Grd[L]): Value = Grds(Set(grd))
        def arr(arr: Arr[L]): Value = Arrs(Set(arr))
        def flt(flt: Flat[L]): Value = Flats(Set(flt))
        def opq(opq: Opq): Value = Opqs(Set(opq))
        def struct(struct: Struct[L]): Value = Structs(Set(struct))
        def structSetterGetter(struct: StructSetterGetter): Value = StructSetterGetters(Set(struct))
        def structConstructor(constr: StructConstructor): Value = StructConstructors(Set(constr))
        def structPredicate(pred: StructPredicate): Value = StructPredicates(Set(pred))

        def getClosures(x: Value): Set[schemeLattice.Closure] = x match
            case Clo(closures) => closures
            case _             => Set.empty
        def getPrimitives(x: Value): Set[String] = x match
            case Prim(prms) => prms
            case _          => Set.empty
        def getContinuations(x: Value): Set[K] = x match
            case Kont(k) => k
            case _       => Set.empty
        def getPointerAddresses(x: Value): Set[A] = x match
            case Pointer(ptrs) => ptrs
            case _             => Set.empty
        def getThreads(x: Value): Set[TID] = x match
            case Thread(t) => t
            case _         => Set.empty
        def getLocks(x: Value): Set[TID] = x match
            case Lock(l) => l
            case _       => Set.empty

        def getBlames(x: Value): Set[Blame] = x match
            case Blames(l) => l
            case _         => Set.empty

        def getGrds(x: Value): Set[Grd[L]] = x match
            case Grds(l) => l
            case _       => Set.empty

        def getArrs(x: Value): Set[Arr[L]] = x match
            case Arrs(l) => l
            case _       => Set.empty

        def getFlats(x: Value): Set[Flat[L]] = x match
            case Flats(l) => l
            case _        => Set.empty

        def getStructs(x: Value): Set[Struct[L]] = x match
            case Structs(l) => l
            case _          => Set.empty

        def getSetterGetters(x: Value): Set[StructSetterGetter] = x match
            case StructSetterGetters(l) => l
            case _                      => Set.empty

        def getStructConstructor(x: Value): Set[StructConstructor] = x match
            case StructConstructors(l) => l
            case _                     => Set.empty

        def getStructPredicates(x: Value): Set[StructPredicate] = x match
            case StructPredicates(l) => l
            case _                   => Set.empty

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

    type L = Elements
    case class Elements(vs: List[Value]) extends SmartHash:
        override def toString: String =
          if vs.isEmpty then "⊥"
          else if vs.tail.isEmpty then vs.head.toString
          else vs.map(_.toString).sorted.mkString("{", ",", "}")
        def foldMapL[X](f: Value => X)(implicit monoid: Monoid[X]): X =
          vs.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))
    object Element extends Serializable:
        def apply(v: Value): L = Elements(List(v))

    import MonoidInstances._
    implicit val lMonoid: Monoid[L] = new Monoid[L] {
      private def insert(vs: List[Value], v: Value): List[Value] = vs match
          case scala.Nil                     => List(v)
          case v0 :: _ if v.ord < v0.ord     => v :: vs
          case v0 :: rest if v.ord == v0.ord => Value.join(v, v0) :: rest
          case v0 :: rest                    => v0 :: insert(rest, v)
      def append(x: L, y: => L): L = (x, y) match
          case (Elements(as), Elements(bs)) => Elements(bs.foldLeft(as)(insert))
      def zero: L = Elements(scala.Nil)
    }
    implicit val lMFMonoid: Monoid[MayFail[L, Error]] = MonoidInstances.mayFail[L]

    val schemeLattice: SchemeLattice[L, A] = new SchemeLattice[L, A] { lat =>
      def show(x: L): String = x.toString /* TODO[easy]: implement better */
      def isTrue(x: L): Boolean = x.foldMapL(Value.isTrue(_))(boolOrMonoid)
      def isFalse(x: L): Boolean = x.foldMapL(Value.isFalse(_))(boolOrMonoid)
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

      def join(x: L, y: => L): L = Monoid[L].append(x, y)
      def subsumes(x: L, y: => L): Boolean =
        y.foldMapL(y =>
          /* For every element in y, there exists an element of x that subsumes it */
          x.foldMapL(x => Value.subsumes(x, y))(boolOrMonoid)
        )(boolAndMonoid)
      def top: L = throw LatticeTopUndefined

      def getClosures(x: L): Set[Closure] = x.foldMapL(x => Value.getClosures(x))(setMonoid)
      def getContinuations(x: L): Set[lat.K] = x.foldMapL(x => Value.getContinuations(x))(setMonoid)
      def getPrimitives(x: L): Set[String] = x.foldMapL(x => Value.getPrimitives(x))(setMonoid)
      def getPointerAddresses(x: L): Set[A] = x.foldMapL(x => Value.getPointerAddresses(x))(setMonoid)
      def getBlames(x: L): Set[Blame] = x.foldMapL(x => Value.getBlames(x))(setMonoid)
      def getGrds(x: L): Set[Grd[L]] = x.foldMapL(x => Value.getGrds(x))(setMonoid)
      def getArrs(x: L): Set[Arr[L]] = x.foldMapL(x => Value.getArrs(x))(setMonoid)
      def getFlats(x: L): Set[Flat[L]] = x.foldMapL(x => Value.getFlats(x))(setMonoid)
      def getStructs(x: L): Set[Struct[L]] = x.foldMapL(x => Value.getStructs(x))(setMonoid)
      def getGetterSetter(x: L): Set[StructSetterGetter] = x.foldMapL(x => Value.getSetterGetters(x))(setMonoid)
      def getStructConstructor(x: L): Set[StructConstructor] = x.foldMapL(x => Value.getStructConstructor(x))(setMonoid)
      def getStructPredicates(x: L): Set[StructPredicate] = x.foldMapL(x => Value.getStructPredicates(x))(setMonoid)
      def getThreads(x: L): Set[TID] = x.foldMapL(Value.getThreads)(setMonoid)
      def acquire(lock: L, tid: TID): MayFail[L, Error] =
        lock.foldMapL(l => Value.acquire(l, tid))
      def release(lock: L, tid: TID): MayFail[L, Error] =
        lock.foldMapL(l => Value.release(l, tid))

      def bottom: L = Elements(List.empty)

      def number(x: BigInt): L = Element(Value.number(x))

      def numTop: L = Element(Int(IntLattice[I].top))
      def charTop: L = Element(Char(CharLattice[C].top))
      def realTop: L = Element(Real(RealLattice[R].top))
      def stringTop: L = Element(Str(StringLattice[S].top))
      def symbolTop: L = Element(Symbol(SymbolLattice[Sym].top))
      def real(x: Double): L = Element(Value.real(x))
      def string(x: String): L = Element(Value.string(x))
      def char(x: scala.Char): L = Element(Value.char(x))
      def bool(x: Boolean): L = Element(Value.bool(x))
      def primitive(x: String): L = Element(Value.primitive(x))
      def closure(x: Closure): L = Element(Value.closure(x))
      def cont(x: lat.K): L = Element(Value.cont(x))
      def symbol(x: String): L = Element(Value.symbol(x))
      def cons(car: L, cdr: L): L = Element(Value.cons(car, cdr))
      def pointer(a: A): L = Element(Value.pointer(a))
      def thread(tid: TID): L = Element(Value.thread(tid))
      def lock(threads: Set[TID]): L = Element(Value.lock(threads))
      def blame(blame: Blame): L = Element(Value.blame(blame))
      def grd(grd: Grd[L]): L = Element(Value.grd(grd))
      def arr(arr: Arr[L]): L = Element(Value.arr(arr))
      def flat(flt: Flat[L]): L = Element(Value.flt(flt))
      def opq(opq: Opq): L = Element(Value.opq(opq))
      def struct(struct: Struct[L]): L = Element(Value.struct(struct))
      def structPredicate(struct: StructPredicate): L = Element(Value.structPredicate(struct))
      def structSetterGetter(setterGetter: StructSetterGetter): L =
        Element(Value.structSetterGetter(setterGetter))
      def structConstructor(constructor: StructConstructor): L =
        Element(Value.structConstructor(constructor))
      def nil: L = Element(Value.nil)
      def void: L = Element(Value.void)
      def eql[B2: BoolLattice](x: L, y: L): B2 = ??? // TODO[medium] implement
      def refs(x: L): Set[Address] = x.foldMapL(x => Value.refs(x))(setMonoid)
      def eq(xs: L, ys: L)(comparePtr: MaybeEq[A]): L =
        xs.foldMapL { x =>
          ys.foldMapL { y =>
            Element(Value.eq(x, y)(comparePtr))
          }
        }
    }

    object L:
        implicit val lattice: SchemeLattice[L, A] = schemeLattice
