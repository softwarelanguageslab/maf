package maf.values.typeclasses

import maf.values.Lattice
import maf.util.*
import maf.values.typeclasses.syntax.*
import cats.extensions.MonadError
import cats.extensions.Errors.raiseError
import cats.syntax.all.*

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

import maf.values.domains.ConstantPropagation.*

/** A very imprecise implementation of the vector lattice */
// case class AbstractVector[L, I](elems: L, siz: I)
// given [L: Lattice]: VectorLattice[AbstractVector[L, CP[Int]], L, CP[Int]] with {
//   def vector[M[_]: MonadError[Error]: MonadJoin](
//       size: CP[Int],
//       init: L
//   ): M[AbstractVector[L, I]] =
//     AbstractVector(init, size).pure
//   def vectorSet[M[_]: MonadError[Error]: MonadJoin](
//       vec: AbstractVector[L, I],
//       pos: CP[Int],
//       vlu: L
//   ): M[AbstractVector[L, I]] =
//     vec.copy(elems = Lattice[L].join(vec.elems, vlu)).pure || raiseError(
//       OutOfBoundsError
//     )
//
//   def vectorRef[M[_]: MonadError[Error]: MonadJoin](
//       vec: AbstractVector[L, CP[Int]],
//       pos: CP[Int]
//   ): M[L] =
//     vec.elems.pure || raiseError(OutOfBoundsError)
//
//   def vectorLength[M[_]: MonadError[Error]: MonadJoin, I: IntLattice](
//       vec: AbstractVector[L, I]
//   ): M[I] = vec.siz.pure
// }
