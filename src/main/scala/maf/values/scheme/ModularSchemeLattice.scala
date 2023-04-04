package maf
package values
package scheme

import interpreter.*
import typeclasses.*
import cats.syntax.all.*
import cats.{~> => _, MonadError => _, _}
import util.*
import types.{*, given}
import maf.syntax.scheme.*
import maf.util.datastructures.*
import domains.*
import cats.extensions.*
import maf.values.typeclasses.Galois.inject
import cats.extensions.Errors.raiseError
import maf.analysis.store.*

//
// Types in a Scheme Value
//

case object IntT extends Key[IntT]
type IntT = IntT.type
case object RealT extends Key[RealT]
type RealT = RealT.type
case object PrimT extends Key[PrimT]
type PrimT = PrimT.type
case object BoolT extends Key[BoolT]
type BoolT = BoolT.type
case object StringT extends Key[StringT]
type StringT = StringT.type
case object CharT extends Key[CharT]
type CharT = CharT.type
case object SymT extends Key[SymT]
type SymT = SymT.type
case object NilT extends Key[NilT]
type NilT = NilT.type
case object UnspT extends Key[UnspT]
type UnspT = UnspT.type
case object CloT extends Key[CloT]
type CloT = CloT.type
case object VecTPtr extends Key[VecTPtr]
type VecTPtr = VecTPtr.type
case object PaiTPtr extends Key[PaiTPtr]
type PaiTPtr = PaiTPtr.type

type ModularSchemeValue[I, R, B, S, C, Sym, V, P] =
    (IntT ~> I) :*:
        (RealT ~> R) :*:
        (BoolT ~> B) :*:
        (StringT ~> S) :*:
        (CharT ~> C) :*:
        (SymT ~> Sym) :*:
        (NilT ~> Unit) :*:
        (UnspT ~> Unit) :*:
        (PrimT ~> Set[String]) :*:
        (CloT ~> Set[(SchemeLambdaExp, Environment[Address])]) :*:
        (VecTPtr ~> Set[VectorAddress[V]]) :*:
        (PaiTPtr ~> Set[PairAddress[P]])

trait ModularSchemeLattice[
    I: IntLattice: GaloisFrom[BigInt],
    R: RealLattice: GaloisFrom[Double],
    B: BoolLattice: GaloisFrom[Boolean],
    S: StringLattice_[I, C, Sym]: GaloisFrom[String],
    C: CharLattice_[I, Sym, S]: GaloisFrom[Char],
    Sym: SymbolLattice,
    Pai,
    Vec]
    extends SchemeLattice[SparseProduct[ModularSchemeValue[I, R, B, S, C, Sym, Vec, Pai]], Vec, Pai]:

    import maf.util.datastructures.ListOps.*

    /** To make sure that Scala finds an instance of ourselves */
    private given SchemeLattice[Val, Vec, Pai] = this

    /** Type alias for convience */
    type Val = SparseProduct[ModularSchemeValue[I, R, B, S, C, Sym, Vec, Pai]]

    //
    // Core lattice operations
    //

    private val joiner = Join[Val#Content]
    private val subsumer = Subsumes[Val#Content]

    def join(x: Val, y: => Val): Val =
        joiner(x, y)

    def subsumes(x: Val, y: => Val): Boolean =
        subsumer(x, y)

    def show(v: Val): String = v.toString
    def top: Val = throw LatticeTopUndefined
    def bottom: Val = SparseProduct.empty[Val#Content]
    def eql[B: BoolLattice: GaloisFrom[Boolean]](x: Val, y: Val): B = ???

    private def insertA[K, V](k: K)(v: V)(using KeyValueIn[K, V, Val#Content]): Val =
        SparseProduct.empty[Val#Content].put(k, v)

    type P = Unit // TODO:

    //
    // Utility functions
    //

    /** Raises an error in the given Monad */
    private def raiseError[M[_]: MonadError, X](error: Error): M[X] =
        ApplicativeError[M, Error].raiseError(error)

    private def setExtractor[K, V <: Set[A], A](k: K)(using KeyValueIn[K, V, Val#Content]): Extractor[Val, A] =
        new Extractor:
            def extract(v: Val): Option[A] =
                if v.isSingleton then
                    v.get(k) match
                        case Some(Singleton(v)) => Some(v)
                        case _                  => None
                else None

    /** Split the value into its smaller parts such that ∀ V, ⋃ { v \in split(V) } \= V
      */
    def split(v: Val): Set[Val] =
        Split[Val#Content](v)

    /** Equality between two values */
    def eq(x: Val, y: Val)(comparePtr: MaybeEq[Address]): Val = ???

    type MonadError[M[_]] = cats.extensions.MonadError[Error][M]

    //
    // Extraction of canonical values
    //

    given galois: Galois[SimpleSchemeValue, Val] with {
        def inject(c: SimpleSchemeValue): Val = c match
            case SchemeNil => insertA(NilT)(())
            case _         => ???
    }

    // Type predicates & extractors
    //

    def primitive: Extractor[Val, String] = setExtractor(PrimT)
    def isPrim[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[PrimT])
    def isStr[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[StringT])
    def isBool[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[BoolT])
    def isReal[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[RealT])
    def isSym[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[SymT])
    def isNull[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[NilT])
    def isInt[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[IntT])
    def isChar[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[CharT])
    def closures: Extractor[Val, (SchemeLambdaExp, VarAddress[Val])] = setExtractor(CloT)
    def isClo[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[CloT])
    def injectClosure(lam: SchemeLambdaExp, env: Environment[VarAddress[Val]]): Val =
        insertA(CloT)(Set((lam, env)))
    def isUnsp[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
        inject(v.contains[UnspT])

    //
    // Pointers to Pairs & vectors
    //

    /** Inject a pointer to a vector */
    def injectVecPtr(adr: VectorAddress[Vec]): Val =
        insertA(VecTPtr)(Set(adr): Set[VectorAddress[Vec]])

    /** Pattern that matches if the abstract value is equal to exactly one address */
    def vectorAddress(v: Val): Extractor[Val, VectorAddress[Vec]] =
        setExtractor(VecTPtr)

    /** Inject a pointer to a vector */
    def injectPairPtr(adr: PairAddress[Pai]): Val =
        insertA(PaiTPtr)(Set(adr): Set[PairAddress[Pai]])

    /** Pattern that matches if the abstract value is equal to exactly one address */
    def pairAddress(v: Val): Extractor[Val, PairAddress[Pai]] =
        setExtractor(PaiTPtr)

    //
    // String operations
    //

    override def length[M[_]: MonadError: MonadJoin](s: Val): M[Val] =
        MonadJoin[M].mfoldMap(split(s)) {
            case StringT(s) => StringLattice[S, I, C, Sym].length(s) map insertA(IntT)
            case v =>
                raiseError(TypeError("string-length: argument is not a string", v))
        }

    override def append[M[_]: MonadError: MonadJoin](s1: Val, s2: Val): M[Val] =
        MonadJoin[M].mfoldMap(split(s1).cartesian(split(s2))) {
            case (StringT(s1), StringT(s2)) =>
                StringLattice[S, I, C, Sym].append(s1, s2) map insertA(StringT)
            case (v1, v2) =>
                raiseError(TypeError("append: arguments must be strings", (v1, v2)))
        }

    override def substring[M[_]: MonadError: MonadJoin](
        s: Val,
        from: Val,
        to: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(
          split(s).cartesian(split(from)).cartesian(split(to))
        ) {
            case ((StringT(s1), IntT(i1)), IntT(i2)) =>
                StringLattice[S, I, C, Sym].substring(s1, i1, i2) map insertA(StringT)
            case v =>
                raiseError(
                  TypeError("substring: arguments must be (string, int, int)", v)
                )
        }

    override def ref[M[_]: MonadError: MonadJoin](s: Val, i: Val): M[Val] =
        MonadJoin[M].mfoldMap(split(s).cartesian(split(i))) {
            case (StringT(s), IntT(i)) =>
                StringLattice[S, I, C, Sym].ref(s, i) map insertA(CharT)
            case (v1, v2) =>
                raiseError(
                  TypeError("string-ref: arguments must be (string, int)", (v1, v2))
                )
        }

    override def set[M[_]: MonadError: MonadJoin](
        s: Val,
        i: Val,
        c: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(s).cartesian(split(i)).cartesian(split(c))) {
            case ((StringT(s), IntT(i)), CharT(c)) =>
                StringLattice[S, I, C, Sym].set(s, i, c) map insertA(StringT)
            case v =>
                raiseError(TypeError("string-set: arguments must be (string, int, char)", v))
        }

    override def stringLt[
        M[_]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](s1: Val,
        s2: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(s1).cartesian(split(s2))) {
            case (StringT(s1), StringT(s2)) =>
                StringLattice[S, I, C, Sym].stringLt(s1, s2)
            case v =>
                raiseError(
                  TypeError("string-ref: arguments must be (string, int, char)", v)
                )
        }

    override def toSymbol[M[_]: MonadError: MonadJoin](s: Val): M[Val] =
        MonadJoin[M].mfoldMap(split(s)) {
            case StringT(s) =>
                StringLattice[S, I, C, Sym].toSymbol(s) map insertA(SymT)
            case v =>
                raiseError(TypeError("string->symbol: expected string", v))
        }

    override def toNumber[M[_]: MonadError: MonadJoin](s: Val): M[Val] =
        MonadJoin[M].mfoldMap(split(s)) {
            case StringT(s) =>
                StringLattice[S, I, C, Sym].toNumber(s) map insertA(IntT)
            case v =>
                raiseError(TypeError("string->number: expected string", v))
        }

    override def makeString[M[_]: MonadError: MonadJoin](
        length: Val,
        char: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(length).cartesian(split(char))) {
            case (IntT(i), CharT(c)) =>
                StringLattice[S, I, C, Sym].makeString(i, c) map insertA(StringT)
            case v =>
                raiseError(TypeError("make-string: expected (int, char)", v))
        }

    //
    // toString
    //

    def toString[
        Sym: SymbolLattice,
        I: IntLattice,
        C: CharLattice_[
          I,
          Sym,
          S
        ],
        S: StringLattice_[I, C, Sym]: GaloisFrom[String]
      ](v: Val
      ): S = ???

    //
    // Char lattice
    //

    override def downCase[M[_]: MonadError: MonadJoin](
        c: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(c)) {
            case CharT(c) =>
                CharLattice[C, I, Sym, S].downCase(c) map insertA(CharT)
            case v => raiseError(TypeError("downCase: expected char", v))
        }

    override def toChar[
        C: CharLattice_[Val, Sym, S]: GaloisFrom[
          Char
        ],
        S: StringLattice_[
          Val,
          C,
          Sym
        ],
        Sym: SymbolLattice
      ](n: Val
      ): C = ???

    def toReal[
        M[_]: MonadError: MonadJoin,
        R: RealLattice: GaloisFrom[
          Double
        ]
      ](n: Val
      ): M[R] =
        MonadJoin[M].mfoldMap(split(n)) {
            case IntT(c) =>
                (IntLattice[I].toReal(c): M[R])
            case v => raiseError(TypeError("toReal: expected integer", v))
        }

    override def upCase[M[_]: MonadError: MonadJoin](
        c: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(c)) {
            case CharT(c) =>
                CharLattice[C, I, Sym, S].upCase(c) map insertA(CharT)
            case v => raiseError(TypeError("upCase: expected char", v))
        }

    override def toInt[
        M[_]: MonadError: MonadJoin,
        I: IntLattice: GaloisFrom[
          BigInt
        ]
      ](c: Val
      ): M[I] =
        MonadJoin[M].mfoldMap(split(c)) {
            case RealT(r) => RealLattice[R].toInt(r)
            case v        => raiseError(TypeError("toInt: expected int", v))
        }

    override def isLower[M[_]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](c: Val): M[B] =
        MonadJoin[M].mfoldMap(split(c)) {
            case CharT(c) =>
                CharLattice[C, I, Sym, S].isLower[M, B](c)
            case v => raiseError(TypeError("isLower: expected char", v))
        }

    override def isUpper[
        M[
            _
        ]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[Boolean]
      ](c: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(c)) {
            case CharT(c) =>
                CharLattice[C, I, Sym, S].isUpper[M, B](c)
            case v => raiseError(TypeError("isUpper: expected char", v))
        }

    override def charEq[
        M[
            _
        ]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[Boolean]
      ](c1: Val,
        c2: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
            case (CharT(c1), CharT(c2)) =>
                CharLattice[C, I, Sym, S].charEq[M, B](c1, c2)
            case v => raiseError(TypeError("charEq: expected char", v))
        }

    override def charLt[
        M[
            _
        ]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[Boolean]
      ](c1: Val,
        c2: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
            case (CharT(c1), CharT(c2)) =>
                CharLattice[C, I, Sym, S].charLt[M, B](c1, c2)
            case v => raiseError(TypeError("charEq: expected char", v))
        }

    override def charEqCI[
        M[
            _
        ]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[Boolean]
      ](c1: Val,
        c2: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
            case (CharT(c1), CharT(c2)) =>
                CharLattice[C, I, Sym, S].charEqCI[M, B](c1, c2)
            case v => raiseError(TypeError("charEq: expected char", v))
        }

    override def charLtCI[
        M[
            _
        ]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[Boolean]
      ](c1: Val,
        c2: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
            case (CharT(c1), CharT(c2)) =>
                CharLattice[C, I, Sym, S].charLtCI[M, B](c1, c2)
            case v => raiseError(TypeError("charEq: expected char", v))
        }

    //
    // Int Lattice
    //

    private def numOperation2[M[_]: MonadError: MonadJoin](
        name: String
      )(intOp: (I, I) => M[I],
        realOp: (R, R) => M[R]
      )(n1: Val,
        n2: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(n1).cartesian(split(n2))) {
            case (IntT(n1), IntT(n2)) => intOp(n1, n2) map insertA(IntT)
            case (IntT(n1), RealT(n2)) =>
                ((IntLattice[I].toReal[M, R](n1), n2.pure) flatMapN ((r1: R, r2: R) => realOp(r1, r2))) map insertA[RealT, R](
                  RealT
                )
            case (RealT(n1), IntT(n2)) =>
                ((n1.pure, IntLattice[I].toReal[M, R](n2)) flatMapN ((r1: R, r2: R) => realOp(r1, r2))) map insertA(RealT)
            case (RealT(n1), RealT(n2)) =>
                realOp(n1, n2) map insertA(RealT)
            case v =>
                raiseError(TypeError(s"$name: expected int or real", v))
        }

    private def numOperation2B[M[_]: MonadError: MonadJoin, B: BoolLattice](
        name: String
      )(intOp: (I, I) => M[B],
        realOp: (R, R) => M[B]
      )(n1: Val,
        n2: Val
      ): M[B] =
        MonadJoin[M].mfoldMap(split(n1).cartesian(split(n2))) {
            case (IntT(n1), IntT(n2)) => intOp(n1, n2)
            case (IntT(n1), RealT(n2)) =>
                ((IntLattice[I].toReal[M, R](n1), n2.pure) flatMapN ((r1: R, r2: R) => realOp(r1, r2)))
            case (RealT(n1), IntT(n2)) =>
                ((n1.pure, IntLattice[I].toReal[M, R](n2)) flatMapN ((r1: R, r2: R) => realOp(r1, r2)))
            case (RealT(n1), RealT(n2)) =>
                realOp(n1, n2)
            case v =>
                raiseError(TypeError(s"$name: expected int or real", v))
        }

    private def numOperation2R[M[_]: MonadError: MonadJoin](
        name: String
      )(intOp: (I, I) => M[R],
        realOp: (R, R) => M[R]
      )(n1: Val,
        n2: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(n1).cartesian(split(n2))) {
            case (IntT(n1), IntT(n2)) => intOp(n1, n2) map insertA(RealT)
            case (IntT(n1), RealT(n2)) =>
                ((IntLattice[I].toReal[M, R](n1), n2.pure) flatMapN ((r1: R, r2: R) => realOp(r1, r2))) map insertA[RealT, R](
                  RealT
                )
            case (RealT(n1), IntT(n2)) =>
                ((n1.pure, IntLattice[I].toReal[M, R](n2)) flatMapN ((r1: R, r2: R) => realOp(r1, r2))) map insertA(RealT)
            case (RealT(n1), RealT(n2)) =>
                realOp(n1, n2) map insertA(RealT)
            case v =>
                raiseError(TypeError(s"$name: expected int or real", v))
        }

    private def numOperation1[M[_]: MonadError: MonadJoin](
        name: String
      )(realOp: R => M[R]
      )(n: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(n)) {
            case IntT(n)  => (IntLattice[I].toReal[M, R](n) >>= realOp) map insertA(RealT)
            case RealT(r) => realOp(r) map insertA(RealT)
            case v        => raiseError(TypeError(s"$name: expected int or real", v))
        }

    private def error2[M[_]: MonadError: MonadJoin, X](
        error: Error
      )(_v1: X,
        _v2: X
      ): M[X] =
        raiseError(error)

    override def quotient[M[_]: MonadError: MonadJoin](
        n1: Val,
        n2: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(n1).cartesian(split(n2))) {
            case (IntT(n1), IntT(n2)) =>
                IntLattice[I].quotient(n1, n2) map insertA(IntT)
            case v =>
                raiseError(TypeError("quotient: expected integer", v))
        }

    override def div[M[_], R: GaloisFrom[Double]](
        n1: Val,
        n2: Val
      )(using
        e1: cats.MonadError[M, Error],
        e2: maf.values.typeclasses.MonadJoin[M],
        e3: maf.values.typeclasses.RealLattice[R]
      ): M[R] = ???

    override def modulo[M[_]: MonadError: MonadJoin](
        n1: Val,
        n2: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(n1).cartesian(split(n2))) {
            case (IntT(n1), IntT(n2)) =>
                IntLattice[I].modulo(n1, n2) map insertA(IntT)
            case v =>
                raiseError(TypeError(s"modulo: expected integers", v))
        }

    override def remainder[M[_]: MonadError: MonadJoin](
        n1: Val,
        n2: Val
      ): M[Val] =
        MonadJoin[M].mfoldMap(split(n1).cartesian(split(n2))) {
            case (IntT(n1), IntT(n2)) =>
                IntLattice[I].remainder(n1, n2) map insertA(IntT)
            case v =>
                raiseError(TypeError(s"remainder: expected integers", v))
        }

    override def valuesBetween(n1: Val, n2: Val): Set[Val] = ???

    //
    // Reals
    //

    override def ceiling[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("ceiling")(RealLattice[R].ceiling)(n)

    override def floor[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("floor")(RealLattice[R].floor)(n)

    override def round[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("round")(RealLattice[R].round)(n)

    override def log[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("log")(RealLattice[R].log)(n)

    override def random[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("random")(RealLattice[R].random)(n)

    override def sin[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("sin")(RealLattice[R].sin)(n)

    override def asin[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("asin")(RealLattice[R].asin)(n)

    override def cos[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("cos")(RealLattice[R].cos)(n)

    override def acos[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("acos")(RealLattice[R].acos)(n)

    override def tan[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("tan")(RealLattice[R].tan)(n)

    override def atan[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("atan")(RealLattice[R].atan)(n)

    override def sqrt[M[_]: MonadError: MonadJoin](n: Val): M[Val] =
        numOperation1("sqrt")(RealLattice[R].sqrt)(n)

    override def plus[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
        numOperation2("plus")(IntLattice[I].plus, RealLattice[R].plus)(n1, n2)

    override def minus[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
        numOperation2("minus")(IntLattice[I].minus, RealLattice[R].minus)(n1, n2)

    override def times[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
        numOperation2("times")(IntLattice[I].times, RealLattice[R].times)(n1, n2)

    override def div[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
        numOperation2R("div")(IntLattice[I].div[M, R], RealLattice[R].div)(n1, n2)

    override def expt[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
        numOperation2("expt")(IntLattice[I].expt, RealLattice[R].expt)(n1, n2)

    // ordering

    override def lt[
        M[_]: MonadError: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](n1: Val,
        n2: Val
      ): M[B] =
        numOperation2B("<")(IntLattice[I].lt, RealLattice[R].lt)(n1, n2)

    // booleans

    override def isTrue(b: Val): Boolean =
        split(b).exists {
            case BoolT(b) => BoolLattice[B].isTrue(b)
            case _        => true
        }

    override def isFalse(b: Val): Boolean =
        split(b).exists {
            case BoolT(b) => BoolLattice[B].isFalse(b)
            case _        => false
        }

    override def not(b: Val): Val =
        val t =
            if isTrue(b) then Galois.inject[Boolean, B](false)
            else BoolLattice[B].bottom
        val f =
            if isFalse(b) then Galois.inject[Boolean, B](true)
            else BoolLattice[B].bottom
        insertA(BoolT)(BoolLattice[B].join(t, f))

    //
    // Symbols
    //

    override def symbol(v: String): Val =
        insertA(SymT)(SymbolLattice[Sym].symbol(v))

//given modularSchemeLattice[Self,
//                           I: IntLattice: GaloisFrom[BigInt],
//                           R: RealLattice: GaloisFrom[Double],
//                           B: BoolLattice: GaloisFrom[Boolean],
//                           S: StringLattice_[I, C, Sym]: GaloisFrom[String],
//                           C: CharLattice_[I, Sym, S]: GaloisFrom[Char],
//                           Sym: SymbolLattice,
//                           Pai: PairLattice_[Self],
//                           Vec
//](using vecLat: VectorLattice[Vec, Self, I], self: Rec[SparseProduct[ModularSchemeValue[I, R, B, S, C, Sym, Vec, Pai]], Self]): SchemeLattice[self.Vlu] =
//    new ModularSchemeDomain[Self, I, R, B, S, C, Sym, Pai, Vec] {}
