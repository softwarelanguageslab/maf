package maf
package values
package scheme

import interpreter.ConcreteSchemeValue
import typeclasses.*
import util.*
import domains.*
import cats.extensions.MonadError

class ModularSchemeDomain[
    I: IntLattice: GaloisFrom[BigInt],
    R: RealLattice: GaloisFrom[Double],
    B: BoolLattice: GaloisFrom[Boolean],
    S: StringLattice_[I, C, Sym]: GaloisFrom[String],
    C: CharLattice_[I, Sym, S]: GaloisFrom[Char],
    Sym: SymbolLattice: GaloisFrom[String]
] extends SchemeDomain:
  type Val = HMap[SchemeTag]
  trait SchemeTag extends LatKey

  given lattice: SchemeLattice[Val] with SparseProductLattice[SchemeTag] with {

    protected case class Tag[C, A: GaloisFrom[C]: Lattice](
        name: String,
        can: SchemeType
    ) extends LatKey.T[C, A],
          SchemeTag

    val IntT = Tag[BigInt, I]("integer", CIntT)
    val RealT = Tag[Double, R]("real", CRealT)
    val BoolT = Tag[Boolean, B]("boolean", CBoolT)
    val StringT = Tag[String, S]("string", CStrT)
    val CharT = Tag[Char, C]("char", CCharT)
    val SymT = Tag[String, Sym]("symbol", CSymT)
    val NilT = Tag[Unit, Unit]("null", CNullT)

    type MonadError[M[_]] = cats.extensions.MonadError[Error][M]

    //
    // Extraction of canonical values
    //

    override def elements(v: Val): List[CanonicalValue] = ???
    given galois: Galois[ConcreteSchemeValue, Val] = ???

    //
    // Type predicates
    //

    def isPrim[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isStr[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isBool[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isReal[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isSym[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isPtr[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isPai[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isNull[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isInt[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isChar[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???
    def isClo[B: BoolLattice: GaloisFrom[Boolean]](v: Val): B = ???

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

    def toString[Sym: SymbolLattice, I: IntLattice, C: CharLattice_[
      I,
      Sym,
      S
    ], S: StringLattice_[I, C, Sym]: GaloisFrom[String]](v: Val): S = ???

    //
    // toString
    //

    override def downCase[M[_]: MonadError: MonadJoin](
        c: Val
    ): M[Val] = ???

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
    ): M[Val] = ???

    override def toInt[M[_]: MonadError: MonadJoin, I: IntLattice: GaloisFrom[
      BigInt
    ]](
        c: Val
    ): M[I] = ???

    override def isLower[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c: Val
    ): M[B] = ???

    override def isUpper[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c: Val
    ): M[B] = ???

    override def charEq[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def charLt[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def charEqCI[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def charLtCI[M[
        _
    ]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](
        c1: Val,
        c2: Val
    ): M[B] = ???

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
