package maf.language.scheme.primitives

import maf.core._
import maf.core.Position._
import maf.language.scheme._
import maf.language.scheme.lattices.SchemeLattice
import maf.core.Monad.MonadSyntaxOps

import Monad._

trait StoreM[M[_], A <: Address, V] extends Monad[M]:
    def addrEq: M[MaybeEq[A]]
    def lookupSto(a: A): M[V]
    def extendSto(a: A, v: V): M[Unit]
    def extendSto(bds: Iterable[(A, V)]): M[Unit] =
        bds.mapM_ { case (adr, vlu) => extendSto(adr, vlu) }
    def updateSto(a: A, v: V): M[Unit]
    // let Scala know to use the instance itself as an implicit
    implicit final private val self: StoreM[M, A, V] = this

trait SchemeAllocM[M[_], A] extends Monad[M]:
    def allocVar(idn: Identifier): M[A]
    def allocPtr(exp: SchemeExp): M[A]

trait SchemePrimM[M[_], A <: Address, V] extends Monad[M] with MonadJoin[M] with MonadError[M, Error] with SchemeAllocM[M, A] with StoreM[M, A, V]:
    // for convenience
    implicit private def self: Monad[M] = this
    def allocVal(exp: SchemeExp, vlu: V): M[A] =
        flatMap(allocPtr(exp))(adr => map(extendSto(adr, vlu))(_ => adr))
    def deref[X: Lattice](x: Set[A])(f: (A, V) => M[X]): M[X] =
        mfoldMap(x)(a => flatMap(lookupSto(a))(v => f(a, v)))
    def allocList(exs: List[SchemeExp], vlus: List[V])(using lat: SchemeLattice[V, A]): M[V] =
        exs.zip(vlus).foldRightM(lat.nil) { case ((ex, vlu), rest) =>
            allocVal(ex, lat.cons(vlu, rest)).map(lat.pointer)
        }
    //def lookupSto(a: A): M[V] = // stop when the lookup of the store is bottom
    //    flatMap(lookupSto(a))(inject)
    // exotic -- not so important if not implemented yet
    def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] = throw new Exception("Not supported")

trait SchemePrimitive[V: Lattice, A <: Address] extends Serializable:
    // Every primitive in Scheme has a unique name
    def name: String
    // They can be called given the calling expression and arguments using any compatible SchemePrimM monad
    def call[M[_]](
        fexp: SchemeExp,
        args: List[V]
      )(implicit m: SchemePrimM[M, A, V]
      ): M[V]

// Primitive-specific errors
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

abstract class SchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A]) extends Serializable:
    def allPrimitives: Map[String, SchemePrimitive[V, A]]
    def apply(name: String): SchemePrimitive[V, A] = allPrimitives(name)
    def ofList(prims: List[SchemePrimitive[V, A]]): Map[String, SchemePrimitive[V, A]] =
        prims.map(prim => (prim.name, prim)).toMap
