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
  type Val = HMap[LatKey]
  val IntT = LatKey.T[BigInt, I]("integer")
  val RealT = LatKey.T[Double, R]("real")
  val BoolT = LatKey.T[Boolean, B]("boolean")
  val StringT = LatKey.T[String, S]("string")
  val CharT = LatKey.T[Char, C]("char")
  val SymT = LatKey.T[String, Sym]("symbol")
  val NilT = LatKey.T[Unit, Unit]("null")

  given lattice: SchemeLattice[Val] with SparseProductLattice[LatKey] with {
    type MonadError[M[_]] = cats.extensions.MonadError[Error][M]

    given galois: Galois[ConcreteSchemeValue, Val] = ???

    //
    // toString
    //

    override def toString[C: CharLattice_[Val, Sym, S], S: StringLattice_[
      Val,
      C,
      Sym
    ]: GaloisFrom[String], Sym: SymbolLattice](n: Val): S = ???

    override def toString[I: IntLattice, C: CharLattice_[
      I,
      Sym,
      S
    ], S: StringLattice_[
      I,
      C,
      Sym
    ]: GaloisFrom[String], Sym: SymbolLattice](n: Val): S = ???

    def toString[I: IntLattice, C: CharLattice_[I, Val, S], S: StringLattice_[
      I,
      C,
      Val
    ]: GaloisFrom[String]](n: Val): S = ???

    override def toString(c: Val): Val = ???

    override def cons(car: Val, cdr: Val): Val = ???
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

    /** Inject a concrete value into the abstract domain, this function
      * corresponds to the `alpha` function in a Galois connection
      */
    override def inject(c: ConcreteSchemeValue): Val = ???

    /** Extract a set concrete values from the abstract domain, this function
      * corresponds to the `gamma` function in a Galois connection
      */
    override def extract(a: Val): Option[Set[ConcreteSchemeValue]] =
      ???

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

    override def lt[M[_]: MonadError: MonadJoin, B: BoolLattice: GaloisFrom[
      Boolean
    ]](
        n1: Val,
        n2: Val
    ): M[B] = ???

    override def isTrue(b: Val): Boolean = ???

    override def isFalse(b: Val): Boolean = ???

    override def isBoolean(b: Val): Val = ???

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
