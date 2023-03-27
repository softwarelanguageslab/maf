package maf.values.typeclasses

import maf.values.Lattice
import maf.util.*
import cats.extensions.MonadError

type VectorLattice_[V, I] = [L] =>> VectorLattice[L, V, I]
trait VectorLattice[L, V, I: IntLattice] extends Lattice[L]:
  def vector[M[_]: MonadError[Error]: MonadJoin](size: I, init: V): M[L]
  def vectorSet[M[_]: MonadError[Error]: MonadJoin](
      vec: L,
      pos: I,
      vlu: V
  ): M[L]
  def vectorRef[M[_]: MonadError[Error]: MonadJoin](vec: L, pos: I): M[V]
  def vectorLength[M[_]: MonadError[Error]: MonadJoin, I: IntLattice](
      vec: L
  ): M[I]
