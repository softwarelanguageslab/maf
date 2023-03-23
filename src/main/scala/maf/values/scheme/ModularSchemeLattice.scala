package maf
package values
package scheme

import interpreter.*
import typeclasses.*
import cats.syntax.all.*
import cats.{MonadError => _, _}
import util.*
import maf.syntax.scheme.*
import domains.*
import cats.extensions.MonadError
import maf.values.typeclasses.Galois.inject
import cats.extensions.Errors.raiseError

class ModularSchemeDomain[
    I: IntLattice: GaloisFrom[BigInt],
    R: RealLattice: GaloisFrom[Double],
    B: BoolLattice: GaloisFrom[Boolean],
    S: StringLattice_[I, C, Sym]: GaloisFrom[String],
    C: CharLattice_[I, Sym, S]: GaloisFrom[Char],
    Sym: SymbolLattice: GaloisFrom[String]
] extends SchemeDomain:
  import maf.util.datastructures.ListOps.*

  /** Type alias for convience */
  type Val = HMap[SchemeTag]

  trait SchemeTag extends LatKey

  given lattice: SchemeLattice[Val] with SparseProductLattice[SchemeTag] with {
    type P = Unit // TODO:

    protected class Tag[C, A: GaloisFrom[C]: Lattice](
        val name: String
    ) extends LatKey.T[C, A],
          SchemeTag:

      def unapply(v: Val): Option[A] =
        v.get(this)

    //
    // Utility functions
    //

    /** Raises an error in the given Monad */
    private def raiseError[M[_]: MonadError, X](error: Error): M[X] =
      ApplicativeError[M, Error].raiseError(error)

    private def setExtractor[C, A <: Set[X], X](
        tag: Tag[C, A]
    ): Extractor[Val, X] =
      (v: Val) =>
        assert(v.isSingleton, "extractor: value not split")
        if v.isSingleton(tag) then
          val vlu: Set[X] = v.get(tag).get
          assert(vlu.size == 1)
          Some(vlu.head)
        else None

    /** Split the value into its smaller parts such that ∀ V, ⋃ { v \in split(V)
      * } = V
      */
    def split(v: Val): Set[Val] =
      v.keys.flatMap(key =>
        key.lat.split(v.get(key).get).map(HMap.empty.put(key, _))
      )

    //
    // Type tags + conversions to canonical representations
    //

    val IntT = new Tag[BigInt, I]("integer")
    val RealT = new Tag[Double, R]("real")
    val PrimT = new Tag[String, Set[String]]("primitive")
    val BoolT = new Tag[Boolean, B]("boolean")
    val StringT = new Tag[String, S]("string")
    val CharT = new Tag[Char, C]("char")
    val SymT = new Tag[String, Sym]("symbol")
    val StrT = new Tag[String, S]("string")
    val NilT = new Tag[Unit, Unit]("null")
    val UnspT = new Tag[Unit, Unit]("unspecified")
    val PaiT = ???
    val PtrT = new Tag[Address, Set[Address]]("pointer")
    val CloT = new Tag[(SchemeExp, Env), Set[(SchemeExp, Env)]]("closure")

    type MonadError[M[_]] = cats.extensions.MonadError[Error][M]

    //
    // Extraction of canonical values
    //

    given galois: Galois[SimpleSchemeValue, Val] with {
      def inject(c: SimpleSchemeValue): Val = c match
        case SchemeInt(i)      => insert(IntT, i)
        case SchemeDouble(d)   => insert(RealT, d)
        case SchemeNil         => insert(NilT, ())
        case SchemeString(s)   => insert(StrT, s)
        case SchemeBoolean(b)  => insert(BoolT, b)
        case SchemeUnspecified => insert(UnspT, ())
    }

    //
    // Type predicates & extractors
    //

    def primitive: Extractor[Val, String] = setExtractor(PrimT)
    def isPrim[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(PrimT))
    def isStr[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(StrT))
    def isBool[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(BoolT))
    def isReal[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(RealT))
    def isSym[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(SymT))
    def isPtr[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(PtrT))
    def isPai[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(PaiT))
    def isNull[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(NilT))
    def isInt[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(IntT))
    def isChar[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(CharT))
    def closures = setExtractor(CloT)
    def isClo[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(CloT))
    def isUnsp[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
      inject(v.contains(UnspT))

    //
    // Pairs
    //

    def cons(car: Val, cdr: Val): Val = ???
    def car[M[_]: MonadError: MonadJoin](v: Val): M[Val] = ???
    def cdr[M[_]: MonadError: MonadJoin](v: Val): M[Val] = ???

    //
    // Pointers & vectors
    //
    def pointer(adr: Address): Val = ???
    def vector[M[_]: MonadError: MonadJoin](
        siz: Val,
        init: Val
    ): M[Val] = ???

    //
    // toString
    //

    def toString[Sym: SymbolLattice, I: IntLattice, C: CharLattice_[
      I,
      Sym,
      S
    ], S: StringLattice_[I, C, Sym]: GaloisFrom[String]](v: Val): S = ???

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

    override def toChar[C: CharLattice_[Val, Sym, S]: GaloisFrom[
      Char
    ], S: StringLattice_[
      Val,
      C,
      Sym
    ], Sym: SymbolLattice](n: Val): C = ???

    def toReal[M[_]: MonadError: MonadJoin, R: RealLattice: GaloisFrom[
      Double
    ]](n: Val): M[R] = ???

    override def upCase[M[_]: MonadError: MonadJoin](
        c: Val
    ): M[Val] =
      MonadJoin[M].mfoldMap(split(c)) {
        case CharT(c) =>
          CharLattice[C, I, Sym, S].upCase(c) map insertA(CharT)
        case v => raiseError(TypeError("upCase: expected char", v))
      }

    override def toInt[M[_]: MonadError: MonadJoin, I: IntLattice: GaloisFrom[
      BigInt
    ]](
        c: Val
    ): M[I] = ???

    override def isLower[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c: Val
    ): M[B] =
      MonadJoin[M].mfoldMap(split(c)) {
        case CharT(c) =>
          CharLattice[C, I, Sym, S].isLower[M, B](c)
        case v => raiseError(TypeError("isLower: expected char", v))
      }

    override def isUpper[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c: Val
    ): M[B] =
      MonadJoin[M].mfoldMap(split(c)) {
        case CharT(c) =>
          CharLattice[C, I, Sym, S].isUpper[M, B](c)
        case v => raiseError(TypeError("isUpper: expected char", v))
      }

    override def charEq[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] =
      MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
        case (CharT(c1), CharT(c2)) =>
          CharLattice[C, I, Sym, S].charEq[M, B](c1, c2)
        case v => raiseError(TypeError("charEq: expected char", v))
      }

    override def charLt[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] =
      MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
        case (CharT(c1), CharT(c2)) =>
          CharLattice[C, I, Sym, S].charLt[M, B](c1, c2)
        case v => raiseError(TypeError("charEq: expected char", v))
      }

    override def charEqCI[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] =
      MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
        case (CharT(c1), CharT(c2)) =>
          CharLattice[C, I, Sym, S].charEqCI[M, B](c1, c2)
        case v => raiseError(TypeError("charEq: expected char", v))
      }

    override def charLtCI[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] =
      MonadJoin[M].mfoldMap(split(c1).cartesian(split(c2))) {
        case (CharT(c1), CharT(c2)) =>
          CharLattice[C, I, Sym, S].charLtCI[M, B](c1, c2)
        case v => raiseError(TypeError("charEq: expected char", v))
      }

    //
    // String lattice
    //

    override def length[M[_]: MonadError: MonadJoin](
        s: Val
    ): M[Val] = ???

    override def append[M[_]: MonadError: MonadJoin](
        s1: Val,
        s2: Val
    ): M[Val] = ???

    override def substring[M[_]: MonadError: MonadJoin](
        s: Val,
        from: Val,
        to: Val
    ): M[Val] = ???

    override def ref[M[_]: MonadError: MonadJoin](
        s: Val,
        i: Val
    ): M[Val] = ???

    override def set[M[_]: MonadError: MonadJoin](
        s: Val,
        i: Val,
        c: Val
    ): M[Val] = ???

    override def toSymbol[M[_]: MonadError: MonadJoin](
        s: Val
    ): M[Val] = ???

    override def toNumber[M[_]: MonadError: MonadJoin](
        s: Val
    ): M[Val] = ???

    override def makeString[M[_]: MonadError: MonadJoin](
        length: Val,
        char: Val
    ): M[Val] = ???

    //
    // Int Lattice
    //

    override def quotient[M[_]: MonadError: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

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
    ): M[Val] = ???

    override def remainder[M[_]: MonadError: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def valuesBetween(n1: Val, n2: Val): Set[Val] = ???

    //
    // Reals
    //

    override def ceiling[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def floor[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def round[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def log[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def random[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def sin[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def asin[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def cos[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def acos[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def tan[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def atan[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def sqrt[M[_]: MonadError: MonadJoin](n: Val): M[Val] = ???

    override def plus[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
      ???

    override def minus[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
      ???

    override def times[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
      ???

    override def div[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
      ???

    override def expt[M[_]: MonadError: MonadJoin](n1: Val, n2: Val): M[Val] =
      ???

    // ordering

    override def lt[M[_]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[
      Boolean
    ]](
        n1: Val,
        n2: Val
    ): M[B] = ???

    // booleans

    override def isTrue(b: Val): Boolean = ???

    override def isFalse(b: Val): Boolean = ???

    override def not(b: Val): Val = ???
  }

//
// Frequently used domains
//

val CP = ConstantPropagation

object ConstantPropagationSchemeDomain
    extends ModularSchemeDomain[
      CP.I,
      CP.R,
      CP.B,
      CP.S,
      CP.C,
      CP.Sym
    ]
