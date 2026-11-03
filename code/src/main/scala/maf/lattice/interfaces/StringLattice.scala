package maf.lattice.interfaces

import maf.core._

case object NotANumberString extends Error

/** A lattice for strings */
trait StringLattice[S] extends Lattice[S]:
    def inject(s: String): S
    def length[I: IntLattice](s: S): I
    def append(s1: S, s2: S): S
    // TODO[easy]: in practice, substring can result in an error and should use MayFail
    def substring[I: IntLattice](
        s: S,
        from: I,
        to: I
      ): S
    // TODO[easy]: in practice, ref can result in an error and should use MayFail
    def ref[I: IntLattice, C: CharLattice](s: S, i: I): C
    // TODO[easy]: in practice, set can result in an error and should use MayFail
    def set[I: IntLattice, C: CharLattice](
        s: S,
        i: I,
        c: C
      ): S
    def lt[B: BoolLattice](s1: S, s2: S): B
    def toSymbol[Sym: SymbolLattice](s: S): Sym
    def toNumber[I: IntLattice](s: S): MayFail[I, Error]

object StringLattice:
    def apply[S: StringLattice]: StringLattice[S] = implicitly
