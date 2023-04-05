package maf.values
package typeclasses

import maf.util.Error
import cats.extensions.*

/** A lattice for integers */
trait IntLattice[I] extends Lattice[I], AsString[I] { self =>
    def toReal[
        M[_]: MonadError[Error]: MonadJoin,
        R: RealLattice: GaloisFrom[
          Double
        ]
      ](n: I
      ): M[R]
    def random[M[_]: MonadError[Error]: MonadJoin](n: I): M[I]
    def plus[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def minus[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def times[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def quotient[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def isZero[B: BoolLattice: GaloisFrom[Boolean]](v: I)(using Galois[BigInt, I]): B =
        eql(v, Galois.inject[BigInt, I](0))

    def div[M[_], R: GaloisFrom[Double]](
        n1: I,
        n2: I
      )(using
        e1: cats.MonadError[M, Error],
        e2: maf.values.typeclasses.MonadJoin[M],
        e3: maf.values.typeclasses.RealLattice[R]
      ): M[R]

    def expt[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def modulo[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def remainder[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
    def lt[
        M[_]: MonadError[Error]: MonadJoin,
        B: BoolLattice: GaloisFrom[
          Boolean
        ]
      ](n1: I,
        n2: I
      ): M[B]
    def valuesBetween(n1: I, n2: I): Set[I]
    def toChar[
        C: CharLattice_[I, Sym, S]: GaloisFrom[Char],
        S: StringLattice_[
          I,
          C,
          Sym
        ],
        Sym: SymbolLattice
      ](n: I
      ): C
}

object IntLattice:
    def apply[I: IntLattice]: IntLattice[I] = implicitly

    // Some default implementations of the IntLattice
    given wrapIntLattice[I: IntLattice, W](using w: Iso[I, W]): IntLattice[W] with {
        private val lat: Lattice[W] = Lattice.wrapLattice[I, W]
        export lat.*
        import cats.syntax.all.*
        import w.{*, given}
        def toReal[M[_$1]: MonadError[Error]: MonadJoin, R: RealLattice: GaloisFrom[Double]](n: W): M[R] =
            IntLattice[I].toReal(n)
        def random[M[_$2]: MonadError[Error]: MonadJoin](n: W): M[W] =
            IntLattice[I].random(n) map wrap
        def plus[M[_$3]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].plus(n1, n2) map wrap
        def minus[M[_$4]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].minus(n1, n2) map wrap
        def times[M[_$5]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].times(n1, n2) map wrap
        def quotient[M[_$6]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].quotient(n1, n2) map wrap
        def div[M[_$7], R: GaloisFrom[Double]](n1: W, n2: W)(implicit e1: MonadError[Error][M], e2: MonadJoin[M], e3: RealLattice[R]): M[R] =
            IntLattice[I].div(n1, n2)
        def expt[M[_$8]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].expt(n1, n2) map wrap
        def modulo[M[_$9]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].modulo(n1, n2) map wrap
        def remainder[M[_$10]: MonadError[Error]: MonadJoin](n1: W, n2: W): M[W] =
            IntLattice[I].remainder(n1, n2) map wrap
        def lt[M[_$11]: MonadError[Error]: MonadJoin, B: BoolLattice: GaloisFrom[Boolean]](n1: W, n2: W): M[B] =
            IntLattice[I].lt(n1, n2)
        def valuesBetween(n1: W, n2: W): Set[W] =
            IntLattice[I].valuesBetween(n1, n2) map wrap
        def toString[Sym: SymbolLattice, I: IntLattice, C: CharLattice_[I, Sym, S], S: StringLattice_[I, C, Sym]: GaloisFrom[String]](v: W): S = ???
        def toChar[
            C: CharLattice_[W, Sym, S]: GaloisFrom[Char],
            S: StringLattice_[
              W,
              C,
              Sym
            ],
            Sym: SymbolLattice
          ](n: W
          ): C = ???
    }
