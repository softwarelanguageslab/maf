package maf.values.typeclasses

import maf.values.Lattice
import maf.util.*
import cats.extensions.MonadError

type VectorLattice_[V, I] = [L] =>> VectorLattice[L, V, I]
trait VectorLattice[L, V, I: IntLattice] extends Lattice[L]:
  def vector[M[_]: MonadError[Error]: MonadJoin](size: I, init: V): M[L]
  def vectorSet[M[_]: MonadError[Error]: MonadJoin](pos: I, vlu: V): M[L]
  def vectorRef[M[_]: MonadError[Error]: MonadJoin](pos: I): M[V]
