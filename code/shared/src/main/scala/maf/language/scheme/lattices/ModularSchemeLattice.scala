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
import maf.language.scheme.lattices.SchemeOp.CharacterUpcase

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
    case object NilT extends ATypeSingleton(Nil)
    case object PointerT extends ATypeSet[A]
    // TODO: make a cons lattice
    // TODO: make a vector lattice
    case object KontT extends ATypeSet[K]
    case object ThreadT extends ATypeSet[TID]
    case object LockT extends ATypeSet[TID]
    case object VoidT extends ATypeSingleton(Void)
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

    def bool(b: B): HMap = HMap.inserted(BoolT, b)

    /** The injected true value */
    val True: L = bool(BoolLattice[B].inject(true))

    /** The injected false value */
    val False: L = bool(BoolLattice[B].inject(false))

    object Value:
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

    /** Checks whether the first and only argument is one of the given abstract types */
    private def hasType(tpy: AbstractType*)(args: List[L]): L =
        Lattice.foldMapL(using HMap.lat)(tpy.toList, t => HMap.inject(BoolT, args(0).isSet(t)))

    private def latOp(f: SchemeLattice[L, A] => (L => L))(args: List[L]): L =
        f(schemeLattice)(args(0))

    import Definition.*

    def coerceun(
        outputR: AbstractType
      )(
        realOp: (R) => outputR.AbstractValue
      )(
        name: String
      )(
        args: List[HMap]
      ): MayFail[HMap, Error] =
        joined(
          (IntT --> outputR |=| ((n: I) => realOp(IntLattice[I].toReal(n)))),
          (RealT --> outputR |=| ((r: R) => realOp(r))),
        )(name)(args)

    def coercebin(
        outputI: AbstractType,
        outputR: AbstractType
      )(
        intOp: (I, I) => outputI.AbstractValue,
        realOp: (R, R) => outputR.AbstractValue
      )(
        name: String
      )(
        args: List[HMap]
      ): MayFail[HMap, Error] =
        joined(
          (IntT -> IntT --> outputI |=| ((n1: I, n2: I) => intOp(n1, n2))),
          (RealT -> RealT --> outputR |=| ((r1: R, r2: R) => realOp(r1, r2))),
          (IntT -> RealT --> outputR |=| ((n1: I, r2: R) => realOp(IntLattice[I].toReal[R](n1), r2))),
          (RealT -> IntT --> outputR |=| ((r1: R, n2: I) => realOp(r1, IntLattice[I].toReal[R](n2))))
        )(name)(args)

    def minus = coercebin(IntT, RealT)(
      IntLattice[I].minus(_, _),
      RealLattice[R].minus(_, _)
    )
    def times = coercebin(IntT, RealT)(
      IntLattice[I].times(_, _),
      RealLattice[R].times(_, _)
    )
    def plus = coercebin(IntT, RealT)(
      IntLattice[I].plus(_, _),
      RealLattice[R].plus(_, _)
    )
    def div = joined(
      (IntT -> IntT --> RealT |=| ((r1: I, r2: I) => IntLattice[I].div(r1, r2))),
      (RealT -> RealT --> RealT |=| ((r1: R, r2: R) => RealLattice[R].div(r1, r2))),
      (IntT -> RealT --> RealT |=| ((r1: I, r2: R) => RealLattice[R].div(IntLattice[I].toReal[R](r1), r2))),
      (RealT -> IntT --> RealT |=| ((r1: R, r2: I) => RealLattice[R].div(r1, IntLattice[I].toReal[R](r2))))
    )
    def quotient = IntT -> IntT --> IntT |=| ((r1: I, r2: I) => IntLattice[I].quotient(r1, r2))
    def expt = coercebin(IntT, RealT)(
      IntLattice[I].expt(_, _),
      RealLattice[R].expt(_, _)
    )
    def modulo = (IntT -> IntT --> IntT |=| ((n1: I, n2: I) => IntLattice[I].modulo(n1, n2)))
    def remainder = (IntT -> IntT --> IntT |=| ((n1: I, n2: I) => IntLattice[I].remainder(n1, n2)))
    def lt = coercebin(BoolT, BoolT)(
      IntLattice[I].lt[B](_, _),
      RealLattice[R].lt[B](_, _)
    )
    def numEq = coercebin(BoolT, BoolT)(
      IntLattice[I].eql[B](_, _),
      RealLattice[R].eql[B](_, _)
    )

    private lazy val ops: Map[SchemeOp, Any] =
        import SchemeOp.*
        Map(
          /** TODO: car, cdr and makeVector, vectorRef and vectorset */
          IsNull -> hasType(NilT),
          IsCons -> hasType(PaiT),
          IsPointer -> hasType(PointerT),
          IsChar -> hasType(CharT),
          IsSymbol -> hasType(SymT),
          IsString -> hasType(StrT),
          IsInteger -> hasType(IntT),
          IsReal -> hasType(RealT),
          IsBoolean -> hasType(BoolT),
          IsNumber -> hasType(RealT, IntT),
          IsVector -> hasType(VectorT),
          IsThread -> hasType(ThreadT),
          IsLock -> hasType(LockT),
          IsProcedure -> hasType(CloT, PrimT),
          IsInputPort -> hasType(InputPortT),
          IsOutputPort -> hasType(OutputPortT),
          Not -> fn(BoolT -> BoolT |=| BoolLattice[B].not(_)) | fn(AnyT -> BoolT |=| (_ => False)),
          //Ceiling -> fn(IntT -> IntT |=| _) | fn(RealT -> RealT |=| RealLattice[R].ceiling(_))
          IsTrue -> latOp(lat => (lat.isTrue andThen (HMap.inject(BoolT, _)))),
          IsFalse -> latOp(_.isFalse),
          Ceiling -> joined(
            IntT --> IntT |=| ((i: I) => i),
            RealT --> RealT |=| ((r: R) => RealLattice[R].ceiling(r))
          ),
          Floor -> joined(
            IntT --> IntT |=| ((i: I) => i),
            RealT --> RealT |=| ((r: R) => RealLattice[R].floor(r))
          ),
          Round -> joined(
            IntT --> IntT |=| ((i: I) => i),
            RealT --> RealT |=| ((r: R) => RealLattice[R].round(r))
          ),
          Log -> coerceun(RealT)(RealLattice[R].log(_)),
          Random -> joined(
            IntT --> IntT |=| ((i: I) => IntLattice[I].random(i)),
            RealT --> RealT |=| ((r: R) => RealLattice[R].random(r))
          ),
          Sin -> coerceun(RealT)(RealLattice[R].sin(_)),
          ASin -> coerceun(RealT)(RealLattice[R].asin(_)),
          Cos -> coerceun(RealT)(RealLattice[R].cos(_)),
          ACos -> coerceun(RealT)(RealLattice[R].acos(_)),
          Tan -> coerceun(RealT)(RealLattice[R].tan(_)),
          ATan -> coerceun(RealT)(RealLattice[R].atan(_)),
          Sqrt -> coerceun(RealT)(RealLattice[R].sqrt(_)),
          // TODO: VectorLength
          StringLength -> (StrT --> IntT |=| ((s: S) => StringLattice[S].length(s))),
          NumberToString -> joined(
            IntT --> StrT |=| ((n: I) => IntLattice[I].toString(n)),
            RealT --> StrT |=| ((r: R) => RealLattice[R].toString(r))
          ),
          // TODO: integrate MayFail from toNumber, and allow for strings to contain floats as well
          // todo: StringToNumber -> (StrT --> IntT |=| ((s: S) => StringLattice[S].toNumber(s).map(Int.apply).getOrElse(throw Exception("invalid number"))),
          ExactToInexact -> joined(
            IntT --> RealT |=| ((i: I) => IntLattice[I].toReal(i)),
            RealT --> RealT |=| ((r: R) => r)
          ),
          InexactToExact -> joined(
            IntT --> IntT |=| ((i: I) => i),
            RealT --> IntT |=| ((r: R) => RealLattice[R].toInt(r)) /* should introduce fractions */
          ),
          CharacterToInteger -> (CharT --> IntT |=| ((c: C) => CharLattice[C].toInt(c))),
          CharacterToString -> (CharT --> StrT |=| ((c: C) => CharLattice[C].toString(c))),
          CharacterDowncase -> (CharT --> CharT |=| ((c: C) => CharLattice[C].downCase(c))),
          CharacterUpcase -> (CharT --> CharT |=| ((c: C) => CharLattice[C].upCase(c))),
          CharacterIsLower -> (CharT --> BoolT |=| ((c: C) => CharLattice[C].isLower(c))),
          CharacterIsUpper -> (CharT --> BoolT |=| ((c: C) => CharLattice[C].isUpper(c))),
          // TODO: makeInputPort & makeOutputPort
          Minus -> minus,
          Times -> times,
          Plus -> plus,
          Div -> div,
          Quotient -> quotient,
          Expt -> expt,
          Modulo -> modulo,
          Remainder -> remainder,
          Lt -> lt,
          NumEq -> numEq,
          StringAppend -> (StrT -> StrT --> StrT |=| ((s1: S, s2: S) => StringLattice[S].append(s1, s2))),
          StringRef -> (StrT -> IntT --> CharT |=| ((s1: S, i: I) => StringLattice[S].ref(s1, i))),
          StringSet -> (StrT -> IntT -> CharT --> StrT |=| ((s1: S, i: I, c: C) => StringLattice[S].set(s1, i, c))),
          StringLt -> (StrT -> StrT --> BoolT |=| ((s1: S, s2: S) => StringLattice[S].lt(s1, s2))),
          CharacterEq -> (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charEq(c1, c2))),
          CharacterLt -> (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charLt(c1, c2))),
          CharacterEqCI -> (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charEqCI(c1, c2))),
          CharacterLtCI -> (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charLtCI(c1, c2))),
          Substring -> (StrT -> IntT -> IntT --> StrT |=| ((s: S, from: I, to: I) => StringLattice[S].substring(s, from, to))),
          MakeString -> (IntT -> CharT --> StrT |=| ((length: I, c: C) => IntLattice[I].makeString(length, c))),
        )

    type L = HMap

    implicit val lMFMonoid: Monoid[MayFail[L, Error]] = MonoidInstances.mayFail[L]

    val schemeLattice: SchemeLattice[L, A] = new SchemeLattice[L, A] { lat =>
        export HMap.lat.*
        def isTrue(x: L): Boolean =
            // something is true if it is (or)
            // (1) an opaque value (because it is equivale to top)
            // (2) the value is a boolean that is true
            // (3) any value different from false, i.e. the abstract value is not value, and if the false boolean where to be removed it is not the only value that remains
            isOpq(x) || x.get(BoolT).map(BoolLattice[B].isTrue).getOrElse(false) || !(x.get(BoolT).map(BoolLattice[B].isFalse).getOrElse(false) && x
                .retracted(BoolT)
                .isEmpty)

        def isFalse(x: L): Boolean =
            isOpq(x) || x.get(BoolT).map(BoolLattice[B].isFalse).getOrElse(false)

        def isBoolean(x: L): Boolean = x.isSet(BoolT)
        def retractBool(x: L): L = ???

        def isOpq(x: L): Boolean = x.isSet(OpqT)

        def op(op: SchemeOp)(args: List[L]): MayFail[L, Error] = ??? // TODO
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
