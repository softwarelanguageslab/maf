package maf.language.scheme.primitives

import maf.core._
import maf.core.Position._
import maf.language.scheme._
import maf.language.CScheme._
import maf.language.scheme.lattices.SchemeLattice

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
    def allocVal(exp: SchemeExp, vlu: V): M[A] =
      flatMap(allocPtr(exp))(adr => map(extendSto(adr, vlu))(_ => adr))
    def deref[X: Lattice](x: Set[A])(f: (A, V) => M[X]): M[X] =
      mfoldMap(x)(a => flatMap(lookupSto(a))(v => f(a, v)))
    // exotic -- not so important if not implemented yet
    def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] = throw new Exception("Not supported")
    def currentThread: M[TID] = throw new Exception("Not supported")

trait SchemePrimitive[V, A <: Address] extends Primitive:
    // Every primitive in Scheme has a unique name
    def name: String
    // They can be called given the calling expression and arguments using any compatible SchemePrimM monad
    def call[M[_]](
        fexp: SchemeExp,
        args: List[V]
      )(implicit m: SchemePrimM[M, A, V]
      ): M[V]
    //
    // Legacy interface
    //
    def callMF[S <: Store](
        fexp: SchemeExp,
        args: List[V],
        store: S,
        scheme: SchemeInterpreterBridge[V, A]
      )(using sto: StoreOps[S, A, V]): MayFail[(V, S), Error] =
        val fn = call[MF[S,A,V]](fexp, args)
        fn(scheme, store).map((res, dlt) => (res, sto.integrate(store, dlt)))

// To support the "old" interface (i.e., for usage in callMF)

trait SchemeInterpreterBridge[V, A <: Address]:
    type Closure = (SchemeLambdaExp, Environment[A])
    def pointer(exp: SchemeExp): A
    def callcc(
        clo: Closure,
        pos: Position
      ): V
    def currentThread: TID

type MF[S <: Store, A <: Address, V] = [X] =>> (bri: SchemeInterpreterBridge[V, A], sto: S) => MayFail[(X, sto.Delta), Error]

given [S <: Store, A <: Address, V](using storeOps: StoreOps[S, A, V]): SchemePrimM[MF[S,A,V], A, V] with
    // shorthands
    type M[X] = MF[S, A, V][X]
    type B = SchemeInterpreterBridge[V, A]
    // monad operations
    def unit[X](x: X): M[X] = 
        (bri: B, sto: S) => MayFail.success((x, storeOps.delta(sto)))
    def map[X, Y](m: M[X])(f: X => Y): M[Y] = 
        (bri: B, sto: S) => m(bri, sto).map((x, d) => (f(x), d))
    def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y] = 
        (bri: B, sto0: S) => m(bri, sto0).flatMap { (x, d0) =>
            val sto1 = storeOps.integrate(sto0, d0)
            f(x)(bri, sto1).map { (y, d1) => 
                (y, storeOps.compose(sto1, d1)(sto0, d0))
            }
        }
    def fail[X](err: Error): M[X] = 
        (bri: B, sto: S) => MayFail.failure(err)
    def allocVar(idn: Identifier): M[A] = 
        throw new Exception("Shouldn't be used here")
    def allocPtr(exp: SchemeExp): M[A] = 
        (bri: B, sto: S) => MayFail.success((bri.pointer(exp), storeOps.delta(sto)))
    def addrEq: M[MaybeEq[A]] = 
        (bri: B, sto: S) => MayFail.success((storeOps.addrEq(sto), storeOps.delta(sto)))
    def lookupSto(a: A): M[V] =
        (bri: B, sto: S) => MayFail.success((sto(a), storeOps.delta(sto)))
    def extendSto(a: A, v: V): M[Unit] = 
        (bri: B, sto: S) => MayFail.success(((), storeOps.extend(sto, a, v)))
    def updateSto(a: A, v: V): M[Unit] = 
        (bri: B, sto: S) => MayFail.success(((), storeOps.update(sto, a, v)))
    def mbottom[X]: M[X] = 
        (bri: B, sto: S) => MayFailError(Set.empty)
    def mjoin[X: Lattice](x: M[X], y: M[X]): M[X] =
        (bri: B, sto: S) => x(bri, sto).join(y(bri, sto), { case ((x1, d1), (x2, d2)) =>
            (Lattice[X].join(x1, x2), storeOps.join(sto, d1, d2))
        })
    // exotic
    override def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] =
        (bri: B, sto: S) => MayFail.success((bri.callcc(clo, pos), storeOps.delta(sto)))
    override def currentThread: M[TID] =
        (bri: B, sto: S) => MayFail.success((bri.currentThread, storeOps.delta(sto)))
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
