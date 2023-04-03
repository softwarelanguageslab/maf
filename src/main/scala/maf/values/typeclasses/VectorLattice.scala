package maf.values.typeclasses

import maf.util.datastructures.ListOps.*
import maf.values.Lattice
import maf.util.*
import maf.values.typeclasses.syntax.*
import cats.extensions.MonadError
import cats.extensions.Errors.raiseError
import cats.syntax.all.*
import maf.values.LatticeTopUndefined

type VectorLattice_[V, I] = [L] =>> VectorLattice[L, V, I]
trait VectorLattice[L, V, I: IntLattice] extends Lattice[L]:
    def vector[M[_]: MonadError[Error]: MonadJoin](size: I, init: V): M[L]
    def vectorSet[M[_]: MonadError[Error]: MonadJoin](
        vec: L,
        pos: I,
        vlu: V
      ): M[L]
    def vectorRef[M[_]: MonadError[Error]: MonadJoin](vec: L, pos: I): M[V]
    def vectorLength[M[_]: MonadError[Error]: MonadJoin](
        vec: L
      ): M[I]

import maf.values.domains.ConstantPropagation.*

/** A very imprecise implementation of the vector lattice */
case class AbstractVector[L, I](elems: L, siz: I)
object AbstractVector:
    given [L: Lattice, I: IntLattice]: VectorLattice[AbstractVector[L, I], L, I] with {
        def show(t: AbstractVector[L, I]): String =
            s"#(size: ${IntLattice[I].show(t.siz)}, elems: {${Lattice[L].show(t.elems)}})"

        def bottom: AbstractVector[L, I] = AbstractVector(Lattice[L].bottom, IntLattice[I].bottom)
        override def isBottom(x: AbstractVector[L, I]): Boolean =
            Lattice[L].isBottom(x.elems) && IntLattice[I].isBottom(x.siz)

        def eql[B: BoolLattice: GaloisFrom[Boolean]](x: AbstractVector[L, I], y: AbstractVector[L, I]): B = ???
        def join(x: AbstractVector[L, I], y: => AbstractVector[L, I]): AbstractVector[L, I] =
            AbstractVector(Lattice[L].join(x.elems, y.elems), Lattice[I].join(x.siz, y.siz))

        def split(v: AbstractVector[L, I]): Set[AbstractVector[L, I]] =
            Lattice[L].split(v.elems).cartesian(Lattice[I].split(v.siz)).map(AbstractVector(_, _)).toSet

        def subsumes(x: AbstractVector[L, I], y: => AbstractVector[L, I]): Boolean =
            Lattice[L].subsumes(x.elems, y.elems) && Lattice[I].subsumes(x.siz, y.siz)

        def top: AbstractVector[L, I] = throw LatticeTopUndefined

        def vector[M[_]: MonadError[Error]: MonadJoin](
            size: I,
            init: L
          ): M[AbstractVector[L, I]] =
            AbstractVector(init, size).pure
        def vectorSet[M[_]: MonadError[Error]: MonadJoin](
            vec: AbstractVector[L, I],
            pos: I,
            vlu: L
          ): M[AbstractVector[L, I]] =
            MonadJoin[M].condM(IntLattice[I].lt[M, CP[Boolean]](pos, vec.siz)) {
                vec.copy(elems = Lattice[L].join(vec.elems, vlu)).pure
            } {
                raiseError(OutOfBoundsError)
            }

        def vectorRef[M[_]: MonadError[Error]: MonadJoin](
            vec: AbstractVector[L, I],
            pos: I
          ): M[L] =
            MonadJoin[M].condM(IntLattice[I].lt[M, CP[Boolean]](pos, vec.siz)) {
                vec.elems.pure
            } {
                raiseError(OutOfBoundsError)
            }

        def vectorLength[M[_]: MonadError[Error]: MonadJoin](
            vec: AbstractVector[L, I]
          ): M[I] = vec.siz.pure
    }
