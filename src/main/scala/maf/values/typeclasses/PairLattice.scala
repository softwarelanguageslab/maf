package maf.values.typeclasses

import maf.values.Lattice
import maf.util.*
import cats.extensions.*

type PairLattice_[V] = [L] =>> PairLattice[L, V]
trait PairLattice[L, V] extends Lattice[L]:
  def cons(a: V, b: V): L
  def car[M[_]: MonadError[Error]: MonadJoin](a: L): M[V]
  def cdr[M[_]: MonadError[Error]: MonadJoin](a: L): M[V]
