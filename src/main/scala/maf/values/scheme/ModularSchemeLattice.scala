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
  val NilT = LatKey.T[Unit, Unit]("null")

  given lattice: SchemeLattice[Val] with SparseProductLattice[LatKey] with {
    //
    // Scheme specific operations
    //

    /** Inject the nil value in the abstract domain */
    def nil: Val = inject(NilT, ())

    /** Inject a pair in the abstract domain */
    def cons(car: Val, cdr: Val): Val = ???

    /** Inject a address as a pointer in the abstract domain */
    def ptr(adr: Address): Val = ???

    //
    // Core lattice operations
    //
    override def remainder[M[_$4]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def isUpper[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        c: Val
    ): M[B] = ???

    override def length[M[_]: MonadError[Error]: MonadJoin](s: Val): M[Val] =
      ???

    override def upCase[M[_]: MonadError[Error]: MonadJoin](c: Val): M[Val] =
      ???

    def toChar[C: CharLattice_[Val, Sym, S], S: StringLattice_[
      Val,
      C,
      Sym
    ], Sym: SymbolLattice](n: Val): C = ???

    override def charEq[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def charLt[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def ref[M[_$2]: MonadError[Error]: MonadJoin](
        s: Val,
        i: Val
    ): M[Val] = ???

    override def isTrue(b: Val): Boolean = ???

    override def ceiling[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] =
      ???

    override def atan[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

    override def asin[M[_$2]: MonadError[Error]: MonadJoin](n: Val): M[Val] =
      ???

    override def modulo[M[_$3]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def tan[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

    override def cos[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

    override def charEqCI[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def toSymbol[M[_]: MonadError[Error]: MonadJoin](s: Val): M[Val] =
      ???

    override def downCase[M[_]: MonadError[Error]: MonadJoin](c: Val): M[Val] =
      ???

    override def valuesBetween(n1: Val, n2: Val): Set[Val] = ???

    override def random[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] =
      ???

    override def set[M[_$3]: MonadError[Error]: MonadJoin](
        s: Val,
        i: Val,
        c: Val
    ): M[Val] = ???

    override def plus[M[_]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def charLtCI[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        c1: Val,
        c2: Val
    ): M[B] = ???

    override def floor[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

    override def not(b: Val): Val = ???

    override def isLower[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        c: Val
    ): M[B] = ???

    override def append[M[_]: MonadError[Error]: MonadJoin](
        s1: Val,
        s2: Val
    ): M[Val] = ???

    override def substring[M[_$1]: MonadError[Error]: MonadJoin](
        s: Val,
        from: Val,
        to: Val
    ): M[Val] = ???

    override def join(x: Val, y: => Val): Val = ???

    override def toReal[M[_]: MonadError[Error]: MonadJoin, R: RealLattice](
        n: Val
    ): M[R] = ???

    override def makeString[M[_]: MonadError[Error]: MonadJoin](
        length: Val,
        char: Val
    ): M[Val] = ???

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

    override def times[M[_]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def minus[M[_]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

    override def lt[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice](
        n1: Val,
        n2: Val
    ): M[B] = ???

    override def sin[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

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

    override def toInt[M[_]: MonadError[Error]: MonadJoin, I: IntLattice](
        n: Val
    ): M[I] = ???

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

    override def round[M[_]: MonadError[Error]: MonadJoin](n: Val): M[Val] = ???

    override def expt[M[_]: MonadError[Error]: MonadJoin](
        n1: Val,
        n2: Val
    ): M[Val] = ???

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
  // Injection
  //

  // override def inject(c: Char): Val = inject(CharT, c)
  // override def injectSymbol(sym: String): Val = inject(SymT, sym)
  // override def injectString(str: String): Val = inject(StringT, str)
  // override def inject(n: Double): Val = inject(RealT, n)
  // override def inject(b: Boolean): Val = inject(BoolT, b)
  // override def inject(n: BigInt): Val = inject(IntT, n)

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
