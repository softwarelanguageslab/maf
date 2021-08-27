package maf.language.scheme.primitives

import maf.core._
import maf.core.Position._
import maf.language.scheme._
import maf.language.CScheme._
import maf.language.scheme.lattices.SchemeLattice

import Monad._

trait StoreM[M[_], A <: Address, V] extends Monad[M] {
  def addrEq: M[MaybeEq[A]]
  def lookupSto(a: A): M[V]
  def extendSto(a: A, v: V): M[Unit]
  def extendSto(bds: Iterable[(A, V)]): M[Unit] =
    bds.mapM_ { case (adr, vlu) => extendSto(adr, vlu) }
  def updateSto(a: A, v: V): M[Unit]
  // let Scala know to use the instance itself as an implicit
  implicit final private val self: StoreM[M, A, V] = this
}

trait SchemeAllocM[M[_], A] extends Monad[M] {
  def allocVar(idn: Identifier): M[A]
  def allocPtr(exp: SchemeExp): M[A]
}

trait SchemePrimM[M[_], A <: Address, V] extends Monad[M] with MonadJoin[M] with MonadError[M, Error] with SchemeAllocM[M, A] with StoreM[M, A, V] {
  // for convenience
  def allocVal(exp: SchemeExp, vlu: V): M[A] =
    flatMap(allocPtr(exp))(adr => map(extendSto(adr, vlu))(_ => adr))
  def deref[X: Lattice](x: Set[A])(f: (A, V) => M[X]): M[X] =
    mfoldMap(x)(a => flatMap(lookupSto(a))(v => f(a, v)))
  // exotic -- not so important if not implemented yet
  def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] = throw new Exception("Not supported")
  def currentThread: M[TID] = throw new Exception("Not supported")
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
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
  def callMF(
      fexp: SchemeExp,
      args: List[V],
      store: Store[A, V],
      scheme: SchemeInterpreterBridge[V, A]
    ): MayFail[(V, store.This), Error] =
    call(fexp, args)(MFInstance(scheme)).run(store)
}

// To support the "old" interface (i.e., for usage in callMF)

trait SchemeInterpreterBridge[V, A <: Address] {
  type Closure = (SchemeLambdaExp, Environment[A])
  def pointer(exp: SchemeExp): A
  def callcc(
      clo: Closure,
      pos: Position
    ): V
  def currentThread: TID
}

trait MF[X, A <: Address, V] {
  def run(sto: Store[A, V]): MayFail[(X, sto.This), Error]
}

// TODO: this can be done much more concisely in Scala 3
case class MFInstance[A <: Address, V](bri: SchemeInterpreterBridge[V, A]) extends SchemePrimM[({ type λ[X] = MF[X, A, V] })#λ, A, V] {
  def unit[X](x: X): MF[X, A, V] = new MF[X, A, V] {
    def run(sto: Store[A, V]) = MayFail.success((x, sto: sto.This))
  }
  def map[X, Y](m: MF[X, A, V])(f: X => Y): MF[Y, A, V] = new MF[Y, A, V] {
    def run(sto: Store[A, V]) = m.run(sto).map { case (res, sto1) => (f(res), sto1) }
  }
  def flatMap[X, Y](m: MF[X, A, V])(f: X => MF[Y, A, V]): MF[Y, A, V] = new MF[Y, A, V] {
    def run(sto: Store[A, V]) = m.run(sto).flatMap { case (x, sto1) => f(x).run(sto1) }
  }
  def fail[X](err: Error): MF[X, A, V] = new MF[X, A, V] {
    def run(sto: Store[A, V]) = MayFail.failure[(X, sto.This), Error](err)
  }
  def allocVar(idn: Identifier): MF[A, A, V] = throw new Exception("NYI")
  def allocPtr(exp: SchemeExp): MF[A, A, V] = unit(bri.pointer(exp))
  def addrEq: MF[MaybeEq[A], A, V] = new MF[MaybeEq[A], A, V] {
    def run(sto: Store[A, V]) = MayFail.success((sto.addrEq, sto: sto.This))
  }
  def lookupSto(a: A): MF[V, A, V] = new MF[V, A, V] {
    def run(sto: Store[A, V]) = sto.lookupMF(a).map((_, sto: sto.This))
  }
  def extendSto(a: A, v: V): MF[Unit, A, V] = new MF[Unit, A, V] {
    def run(sto: Store[A, V]) = MayFail.success(((), sto.extend(a, v)))
  }
  def updateSto(a: A, v: V): MF[Unit, A, V] = new MF[Unit, A, V] {
    def run(sto: Store[A, V]) = MayFail.success(((), sto.update(a, v)))
  }
  def mbottom[X]: MF[X, A, V] = new MF[X, A, V] {
    def run(sto: Store[A, V]) = MayFailError[(X, sto.This), Error](Set.empty)
  }
  def mjoin[X: Lattice](x: MF[X, A, V], y: MF[X, A, V]): MF[X, A, V] = new MF[X, A, V] {
    def run(sto: Store[A, V]) =
      x.run(sto.deltaStore)
        .join(y.run(sto.deltaStore),
              (xres, yres) => {
                (Lattice[X].join(xres._1, yres._1), xres._2.join(yres._2))
              }
        )
        .map { case (v, delta) => (v, sto.integrate(delta)) }
  }
  // exotic
  override def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): MF[V, A, V] = 
    unit(bri.callcc(clo, pos))
  override def currentThread: MF[TID, A, V] = 
    unit(bri.currentThread)
}

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

abstract class SchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A]) extends Serializable {
  def allPrimitives: Map[String, SchemePrimitive[V, A]]
  def apply(name: String): SchemePrimitive[V, A] = allPrimitives(name)
  def ofList(prims: List[SchemePrimitive[V, A]]): Map[String, SchemePrimitive[V, A]] =
    prims.map(prim => (prim.name, prim)).toMap
}
