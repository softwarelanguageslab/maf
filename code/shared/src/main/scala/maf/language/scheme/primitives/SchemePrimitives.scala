package maf.language.scheme.primitives

import maf.core._
import maf.core.Position._
import maf.language.scheme._
import maf.language.CScheme._
import maf.language.scheme.lattices.SchemeLattice
import maf.lattice.interfaces.BoolLattice

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
    def callMF(
        fexp: SchemeExp,
        args: List[V]
      )(using bridge: SchemeInterpreterBridge[V, A]
      ): MayFail[V, Error] =
      call[MF](fexp, args)

// To support the "old" interface (i.e., for usage in callMF)

trait SchemeInterpreterBridge[V, A <: Address]:
    type Closure = (SchemeLambdaExp, Environment[A])
    def pointer(exp: SchemeExp): A
    def callcc(
        clo: Closure,
        pos: Position
      ): V
    def readSto(a: A): V
    def writeSto(a: A, v: V): Unit
    def currentThread: TID
    def addrEq: MaybeEq[A] =
      new MaybeEq[A] {
        def apply[B: BoolLattice](a1: A, a2: A) =
          if a1 == a2 then BoolLattice[B].top // we don't know (could be different concrete addresses abstracted to the same abstract address)
          else BoolLattice[B].inject(false) // definitely not the same address
      }

type MF[X] = MayFail[X, Error]
given MFInstance[A <: Address, V](using bri: SchemeInterpreterBridge[V, A]): SchemePrimM[MF, A, V] with
    def unit[X](x: X): MF[X] = MayFail.success(x)
    def map[X, Y](m: MF[X])(f: X => Y): MF[Y] = m.map(f)
    def flatMap[X, Y](m: MF[X])(f: X => MF[Y]): MF[Y] = m.flatMap(f)
    def fail[X](err: Error): MF[X] = MayFail.failure(err)
    def allocVar(idn: Identifier): MF[A] = throw new Exception("Shouldn't be used here")
    def allocPtr(exp: SchemeExp): MF[A] = MayFail.success(bri.pointer(exp))
    def addrEq: MF[MaybeEq[A]] = MayFail.success(bri.addrEq)
    def lookupSto(a: A): MF[V] = MayFail.success(bri.readSto(a))
    def extendSto(a: A, v: V) = MayFail.success(bri.writeSto(a, v))
    def updateSto(a: A, v: V) = MayFail.success(bri.writeSto(a, v))
    def mbottom[X]: MF[X] = MayFailError(Set.empty)
    def mjoin[X: Lattice](x: MF[X], y: MF[X]): MF[X] = x.join(y, Lattice[X].join)
    override def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): MF[V] = MayFail.success(bri.callcc(clo, pos))
    override def currentThread: MF[TID] = MayFail.success(bri.currentThread)

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
