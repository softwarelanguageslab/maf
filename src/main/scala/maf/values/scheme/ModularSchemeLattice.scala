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

  given lattice: SchemeLattice[Val]
    with SparseProductLattice[LatKey]
    with {
      //
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
