package maf
package values
package scheme

import interpreter.*
import typeclasses.*
import cats.syntax.all.*
import cats.{MonadError => _, ~> => _, _}
import util.*
import maf.syntax.scheme.*
import maf.util.datastructures.*
import domains.*
import cats.extensions.MonadError
import maf.values.typeclasses.Galois.inject
import cats.extensions.Errors.raiseError
import maf.util.types.{given, *}

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
case object PaiT extends Key[PaiT]
type PaiT = PaiT.type
case object PtrT extends Key[PtrT]
type PtrT = PtrT.type
case object CloT extends Key[CloT]
type CloT = CloT.type

// TODO:
type Environment = Unit

type ModularSchemeValue[I, R, B, S, C, Sym] =
  (IntT ~> I) :*:
    (RealT ~> R) :*:
    (BoolT ~> B) :*:
    (StringT ~> S) :*:
    (CharT ~> C) :*:
    (SymT ~> Sym) :*:
    (NilT ~> Unit) :*:
    (UnspT ~> Unit) :*:
    (PrimT ~> Set[String]) :*:
    (CloT ~> Set[(SchemeExp, Environment)]) :*:
    (PtrT ~> Set[Address])

given ModularSchemeDomain[
    I: IntLattice: GaloisFrom[BigInt],
    R: RealLattice: GaloisFrom[Double],
    B: BoolLattice: GaloisFrom[Boolean],
    S: StringLattice_[I, C, Sym],
    C: CharLattice_[I, Sym, S]: GaloisFrom[Char],
    Sym: SymbolLattice: GaloisFrom[String],
    Pai: PairLattice_[SparseProduct[ModularSchemeValue[I, R, B, S, C, Sym]]],
    Vec: VectorLattice_[SparseProduct[
      ModularSchemeValue[I, R, B, S, C, Sym]
    ], I]
]: SchemeLattice[SparseProduct[ModularSchemeValue[I, R, B, S, C, Sym]]] with
  import maf.util.datastructures.ListOps.*

  /** Type alias for convience */
  type Val = SparseProduct[ModularSchemeValue[I, R, B, S, C, Sym]]

  //
  // Core lattice operations
  //

  private val joiner = Join[Val#Content]
  private val subsumer = Subsumes[Val#Content]

  def join(x: Val, y: => Val): Val =
    joiner(x, y)

  def subsumes(x: Val, y: => Val): Boolean =
    subsumer(x, y)

  private def insertA[K, V](k: K)(v: V)(using
      KeyValueIn[K, V, Val#Content]
  ): Val =
    SparseProduct.empty[Val#Content].put(k, v)

  type P = Unit // TODO:

  //
  // Utility functions
  //

  /** Raises an error in the given Monad */
  private def raiseError[M[_]: MonadError, X](error: Error): M[X] =
    ApplicativeError[M, Error].raiseError(error)

  private def setExtractor[K, V <: Set[A], A](k: K)(using
      KeyValueIn[K, V, Val#Content]
  ): Extractor[Val, A] =
    new Extractor:
      def unapply(v: Val): Option[A] =
        if v.isSingleton then
          v.get(k) match
            case Some(Singleton(v)) => Some(v)
            case _                  => None
        else None

  /** Split the value into its smaller parts such that ∀ V, ⋃ { v \in split(V) }
    * \= V
    */
  def split(v: Val): Set[Val] =
    Split[Val#Content](v)

  type MonadError[M[_]] = cats.extensions.MonadError[Error][M]

  //
  // Extraction of canonical values
  //

  given galois: Galois[SimpleSchemeValue, Val] = ???

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
  def isPtr[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
    inject(v.contains[PtrT])
  def isNull[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
    inject(v.contains[NilT])
  def isInt[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
    inject(v.contains[IntT])
  def isChar[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
    inject(v.contains[CharT])
  def closures = setExtractor(CloT)
  def isClo[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
    inject(v.contains[CloT])
  def isUnsp[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B =
    inject(v.contains[UnspT])

  //
  // Pointers & vectors
  //

  def pointer(adr: Address): Val =
    insertA(PtrT)(Set(adr): Set[Address])

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
  ]](n: Val): M[R] =
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

  override def toInt[M[_]: MonadError: MonadJoin, I: IntLattice: GaloisFrom[
    BigInt
  ]](
      c: Val
  ): M[I] =
    MonadJoin[M].mfoldMap(split(c)) {
      case RealT(r) => RealLattice[R].toInt(r)
      case v        => raiseError(TypeError("toInt: expected int", v))
    }

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

//
// Frequently used domains
//

import ConstantPropagation.*
type CPSchemeValue = ModularSchemeValue[
  CP[BigInt],
  CP[Double],
  CP[Boolean],
  CP[String],
  CP[Char],
  CP[Symbol]
]
