package maf
package values
package scheme

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

  given lattice: SchemeLattice[Val] with SparseProductLattice[LatKey] with {
    override def remainder[M[_$4]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def isUpper[B: BoolLattice](c: Val): B = ???

    override def length(s: Val): Val = ???

    override def upCase(c: Val): Val = ???

    def toChar[C: CharLattice_[Val, Sym, S], S: StringLattice_[
      Val,
      C,
      Sym
    ], Sym: SymbolLattice](n: Val): C = ???

    override def atan(n: Val): Val = ???

    override def asin[M[_$2]: MonadError[Error]: MonadJoin](n: Val): M[Val] =
      ???

    override def charEq[B: BoolLattice](c1: Val, c2: Val): B = ???

    override def charLt[B: BoolLattice](c1: Val, c2: Val): B = ???

    override def ref[M[_$2]: MonadError[Error]: MonadJoin](
        s: Val,
        i: Val
    ): M[Val] = ???

    override def isTrue(b: Val): Boolean = ???

    override def ceiling(n: Val): Val = ???

    override def modulo[M[_$3]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def tan(n: Val): Val = ???

    override def cos(n: Val): Val = ???

    override def charEqCI[B: BoolLattice](c1: Val, c2: Val): B = ???

    override def toSymbol(s: Val): Val = ???

    override def downCase(c: Val): Val = ???

    override def valuesBetween(n1: Val, n2: Val): Set[Val] = ???

    override def random(n: Val): Val = ???

    override def set[M[_$3]: MonadError[Error]: MonadJoin](
        s: Val,
        i: Val,
        c: Val
    ): M[Val] = ???

    override def plus(n1: Val, n2: Val): Val = ???

    override def charLtCI[B: BoolLattice](c1: Val, c2: Val): B = ???

    override def floor(n: Val): Val = ???

    override def not(b: Val): Val = ???

    override def isLower[B: BoolLattice](c: Val): B = ???

    override def append(s1: Val, s2: Val): Val = ???

    override def subsumes(x: Val, y: => Val): Boolean = ???

    override def substring[M[_$1]: MonadError[Error]: MonadJoin](
        s: Val,
        from: Val,
        to: Val
    ): M[Val] = ???

    override def join(x: Val, y: => Val): Val = ???

    override def toReal[R: RealLattice](n: Val): R = ???

    override def makeString(length: Val, char: Val): Val = ???

    override def log[M[_$1]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

    override def div[M[_$5]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def div[M[_$2], R](n1: Val, n2: Val)(using
        e1: cats.MonadError[M, Error],
        e2: MonadJoin[M],
        e3: RealLattice[R]
    ): M[R] = ???

    override def times(n1: Val, n2: Val): Val = ???

    override def minus(n1: Val, n2: Val): Val = ???

    override def lt[B: BoolLattice](n1: Val, n2: Val): B = ???

    override def sin(n: Val): Val = ???

    override def toString(c: Val): Val = ???

    override def toString[I: IntLattice, C: CharLattice_[
      I,
      Val,
      S
    ], S: StringLattice_[I, C, Val]](n: Val): S = ???

    override def toString[I: IntLattice, C: CharLattice_[
      I,
      Sym,
      S
    ], S: StringLattice_[I, C, Sym], Sym: SymbolLattice](n: Val): S = ???

    override def toString[C: CharLattice_[Val, Sym, S], S: StringLattice_[
      Val,
      C,
      Sym
    ], Sym: SymbolLattice](n: Val): S = ???

    override def toInt[I: IntLattice](n: Val): I = ???

    override def acos[M[_$3]: MonadError[Error]: MonadJoin](n: Val): M[Val] =
      ???

    override def quotient[M[_$1]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def eql[B: BoolLattice](x: Val, y: Val): B = ???

    override def toNumber[M[_$4]: MonadError[Error]: MonadJoin](
        s: Val
    ): M[Val] = ???

    override def isFalse(b: Val): Boolean = ???

    override def sqrt[M[_$4]: MonadError[Error]: MonadJoin](n: Val): M[Val] =
      ???

    override def round(n: Val): Val = ???

    override def expt(n1: Val, n2: Val): Val = ???

    //
    // Injection
    //

    override def inject(c: Char): Val = inject(CharT, c)
    override def injectSymbol(sym: String): Val = inject(SymT, sym)
    override def injectString(str: String): Val = inject(StringT, str)
    override def inject(n: Double): Val = inject(RealT, n)
    override def inject(b: Boolean): Val = inject(BoolT, b)
    override def inject(n: BigInt): Val = inject(IntT, n)

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
