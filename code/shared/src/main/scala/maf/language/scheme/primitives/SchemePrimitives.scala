package maf.language.scheme.primitives

import maf.core._
import maf.core.Position._
import maf.language.scheme._
import maf.language.CScheme._
import maf.language.scheme.lattices.SchemeLattice

trait SchemeInterpreterBridge[V, A <: Address] {
  type Closure = (SchemeLambdaExp, Environment[A])
  def pointer(exp: SchemeExp): A
  def callcc(
      clo: Closure,
      pos: Position
    ): V
  def currentThread: TID
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
  // Every primitive in Scheme has a unique name
  def name: String
  // They can be called given the arguments, expressions, store and some interface to the Scheme interpreter
  def call(
      fexp: SchemeExp,
      args: List[(SchemeExp, V)],
      store: Store[A, V],
      scheme: SchemeInterpreterBridge[V, A]
    ): MayFail[(V, store.This), Error]
}

case class PrimitiveArityError(
    name: String,
    expected: Int,
    got: Int)
    extends Error
case class PrimitiveVariadicArityError(
    name: String,
    expectedAtLeast: Int,
    got: Int)
    extends Error
case class PrimitiveNotApplicable[V](name: String, args: List[V]) extends Error
case class UserError(message: String) extends Error

abstract class SchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A]) extends Serializable {
  def allPrimitives: Map[String, SchemePrimitive[V, A]]
  def apply(name: String): SchemePrimitive[V, A] = allPrimitives(name)
  def ofList(prims: List[SchemePrimitive[V, A]]): Map[String, SchemePrimitive[V, A]] =
    prims.map(prim => (prim.name, prim)).toMap
}
