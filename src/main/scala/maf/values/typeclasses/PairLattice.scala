package maf.values.typeclasses

import maf.util.datastructures.ListOps.*
import maf.values.Lattice
import maf.util.*
import cats.extensions.*
import cats.syntax.all.*
import maf.values.LatticeTopUndefined
import scala.concurrent.ExecutionContext

type PairLattice_[V] = [L] =>> PairLattice[L, V]
trait PairLattice[L, V] extends Lattice[L]:
    def cons(a: V, b: V): L
    def car[M[_]: MonadError[Error]: MonadJoin](a: L): M[V]
    def cdr[M[_]: MonadError[Error]: MonadJoin](a: L): M[V]

object PairLattice:
    def apply[L, V](using p: PairLattice[L, V]): PairLattice[L, V] = p

case class AbstractPair[L](car: L, cdr: L)
object AbstractPair:
    given simplePairLattice[L: Lattice]: PairLattice[AbstractPair[L], L] with {
        def show(t: AbstractPair[L]): String =
            s"(${Lattice[L].show(t.car)} . ${Lattice[L].show(t.cdr)})"

        def bottom: AbstractPair[L] =
            AbstractPair(Lattice[L].bottom, Lattice[L].bottom)
        override def isBottom(x: AbstractPair[L]): Boolean =
            Lattice[L].isBottom(x.car) && Lattice[L].isBottom(x.cdr)

        def eql[B: BoolLattice: GaloisFrom[Boolean]](
            x: AbstractPair[L],
            y: AbstractPair[L]
          ): B = ???
        def join(x: AbstractPair[L], y: => AbstractPair[L]): AbstractPair[L] =
            AbstractPair(Lattice[L].join(x.car, y.car), Lattice[L].join(x.cdr, y.cdr))
        def split(v: AbstractPair[L]): Set[AbstractPair[L]] =
            Lattice[L]
                .split(v.car)
                .cartesian(Lattice[L].split(v.cdr))
                .map(AbstractPair(_, _))
                .toSet
        def subsumes(x: AbstractPair[L], y: => AbstractPair[L]): Boolean =
            Lattice[L].subsumes(x.car, y.car) && Lattice[L].subsumes(x.cdr, y.cdr)
        def top: AbstractPair[L] = throw LatticeTopUndefined
        def car[M[_]: MonadError[Error]: MonadJoin](a: AbstractPair[L]): M[L] =
            a.car.pure
        def cdr[M[_]: MonadError[Error]: MonadJoin](a: AbstractPair[L]): M[L] =
            a.cdr.pure
        def cons(a: L, b: L): AbstractPair[L] = AbstractPair(a, b)
    }
