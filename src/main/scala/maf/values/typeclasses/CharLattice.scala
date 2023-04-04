package maf.values
package typeclasses

import maf.util.Error
import cats.extensions.*

type CharLattice_[I, Sym, S] = [C] =>> CharLattice[C, I, Sym, S]

/** A lattice for characters */
trait CharLattice[C, I, Sym, S] extends Lattice[C], AsString[C]:
    def downCase[M[_]: MonadError[Error]: MonadJoin](c: C): M[C]
    def upCase[M[_]: MonadError[Error]: MonadJoin](c: C): M[C]
    def toInt[
        M[_]: MonadError[Error]: MonadJoin,
        I: IntLattice: GaloisFrom[
          BigInt
        ]
      ](c: C
      ): M[I]

    def isLower[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](c: C
      ): M[B]
    def isUpper[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](c: C
      ): M[B]

    def charEq[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](c1: C,
        c2: C
      ): M[B]
    def charLt[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](c1: C,
        c2: C
      ): M[B]

    def charEqCI[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](c1: C,
        c2: C
      ): M[B]
    def charLtCI[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](c1: C,
        c2: C
      ): M[B]

object CharLattice:
    def apply[
        C: CharLattice_[
          I,
          Sym,
          S
        ],
        I: IntLattice,
        Sym: SymbolLattice,
        S: StringLattice_[
          I,
          C,
          Sym
        ]
      ]: CharLattice[C, I, Sym, S] = summon
