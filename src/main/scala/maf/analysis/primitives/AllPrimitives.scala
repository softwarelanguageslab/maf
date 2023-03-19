package maf
package analysis.primitives

import cats.{MonadError => _, *}
import cats.extensions.*
import maf.syntax.scheme.*
import util.*
import values.*
import values.scheme.*
import values.typeclasses.*

trait SchemePrimitive[V: Lattice, A <: Address]:
  // Every primitive in Scheme has a unique name
  def name: String
  // They can be called given the calling expression and arguments using any compatible SchemePrimM monad
  def call[M[_]: Monad: MonadError[Error]](
      fexp: SchemeExp,
      args: List[V]
  )(implicit m: SchemePrimM[M, A, V]): M[V]
