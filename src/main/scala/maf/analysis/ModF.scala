package maf.analysis

import cats.{MonadError => _, *}
import maf.values.typeclasses.MonadJoin
import cats.data.OptionT
import cats.extensions.MonadError
import maf.values.Lattice
import cats.syntax.all.*
import cats.data.StateT
import maf.analysis.store.Store
import maf.analysis.primitives.*
import maf.analysis.store.*
import maf.util.MaybeEq
import maf.syntax.scheme.SchemeExp
import maf.util.*
import maf.analysis.typeclasses.*
import cats.data.Kleisli

// type M = (ErrorT (StoT (EnvT (CtxT (AllocT Id)))))

//
// Components
//

/** A component that is analysed in isolation from the other components
  *
  * @tparam Ctx
  *   an optional context of the component, used to differentiate components that are syntactically the same but might differ at runtime
  */
trait Component[+Ctx]

/** A component corresponding to a call to some closure */
case class CallComponent[Ctx](clo: (SchemeExp, Environment[Address]), ctx: Ctx) extends Component[Ctx]

// Effects

trait Effect

/** An effect on an address */
case class AddressDependency(adr: Address) extends Effect

//
// Error handling
//

type ErrorT[M[_]] = [A] =>> M[(Set[maf.util.Error], Option[A])]

given errorMonad[M[_]: Monad]: Monad[ErrorT[M]] with {
    def pure[A](x: A): ErrorT[M][A] = (Set(), Some(x)).pure
    def flatMap[A, B](fa: ErrorT[M][A])(f: A => ErrorT[M][B]): ErrorT[M][B] =
        Monad[M].flatMap(fa)((v: (Set[maf.util.Error], Option[A])) =>
            v match
                case (e1, Some(a)) => Monad[M].map(f(a)) { case (e2, b) => (e1 ++ e2, b) }
                case (e1, _)       => (e1, None).pure
        )
    def tailRecM[A, B](a: A)(f: A => ErrorT[M][Either[A, B]]): ErrorT[M][B] = ???
}

given [M[_]: MonadJoin: Monad]: MonadJoin[ErrorT[M]] with {
    def mjoin[X: Lattice](a: ErrorT[M][X], b: ErrorT[M][X]): ErrorT[M][X] =
        MonadJoin[M].mjoin(a, b)
}

given [M[_]: Monad]: MonadError[maf.util.Error][ErrorT[M]] with {
    val m: Monad[ErrorT[M]] = errorMonad[M]
    export m.{flatMap, pure, tailRecM}

    def handleErrorWith[A](fa: ErrorT[M][A])(f: maf.util.Error => ErrorT[M][A]): ErrorT[M][A] =
        throw Exception("ErrorT does not allow error handlers") // TODO

    def raiseError[A](e: maf.util.Error): ErrorT[M][A] =
        (Set(e), None: Option[A]).pure
}

//
// Global store
//

/** Keep track of a global store using a state monad
  *
  * @tparam M
  *   a monad inside the state monad
  * @tparam S
  *   the type of the store that is kept around
  */
type StoreT[M[_], S] = [A] =>> StateT[M, S, A]

/** The StoreT monad transformer implements the `StoreM` interface if the store `S` supports the `Store` typeclass */
given [M[_]: Monad, S, V](using Store[S]): StoreM[StoreT[M, S], Address, V] with {
    def lookupSto[A <: StoreAddress](adr: A): StoreT[M, S][adr.Value] =
        StateT.get.map(_.lookup(adr))
    def addrEq: StoreT[M, S][MaybeEq[Address]] = ???
    def updateSto[A <: StoreAddress](adr: A, v: adr.Value): StoreT[M, S][Unit] =
        StateT.modify(_.update(adr, v))
    def extendSto[A <: StoreAddress](adr: A, v: adr.Value): StoreT[M, S][Unit] =
        StateT.modify(_.extend(adr, v))
}

//
// Tracking effects
//

/** A state to keep track of the set of effects generated during an intra analysis */
case class EffectState[Ctx](reads: Set[Effect], writes: Set[Effect], calls: Set[Component[Ctx]]):
    /** Register a dependency on the given effect */
    def register(eff: Effect): EffectState[Ctx] =
        this.copy(reads = reads + eff)

    /** Trigger the dependencies of this effect */
    def trigger(eff: Effect): EffectState[Ctx] =
        this.copy(writes = writes + eff)

type EffectT[Ctx, M[_]] = [A] =>> StateT[M, EffectState[Ctx], A]

/** Tracks effects on the global store */
given [M[_]: Monad, V, Ctx](using storeM: StoreM[M, Address, V]): StoreM[EffectT[Ctx, M], V, Address] with {
    def lookupSto[A <: StoreAddress](adr: A): EffectT[Ctx, M][adr.Value] =
        // lookup registers a read effect and forwards the read to the inner monad
        StateT.modify((eff: EffectState[Ctx]) => eff.register(AddressDependency(adr))) >> StateT.lift(storeM.lookupSto(adr))

    def addrEq: EffectT[Ctx, M][MaybeEq[V]] = ???

    def updateSto[A <: StoreAddress](adr: A, v: adr.Value): EffectT[Ctx, M][Unit] =
        // update registers a write effect if it actually changes the contents of the store
        for
            vlu <- StateT.lift(storeM.lookupSto(adr))
            // only trigger the effect if the value has changed
            _ <-
                if adr.lattice.subsumes(vlu, v) then ().pure[EffectT[Ctx, M]]
                else StateT.modify((e: EffectState[Ctx]) => e.trigger(AddressDependency(adr)))
            // update the underlying store
            _ <- StateT.lift(storeM.updateSto(adr, v))
        yield ()

    def extendSto[A <: StoreAddress](adr: A, v: adr.Value): EffectT[Ctx, M][Unit] =
        // update registers a write effect if it actually changes the contents of the store
        for
            vlu <- StateT.lift(storeM.lookupSto(adr))
            // only trigger the effect if the value has changed
            _ <-
                if adr.lattice.subsumes(vlu, v) then ().pure[EffectT[Ctx, M]]
                else StateT.modify((e: EffectState[Ctx]) => e.trigger(AddressDependency(adr)))
            // extend the underlying store
            _ <- StateT.lift(storeM.extendSto(adr, v))
        yield ()

}

//
// Environment & Context
//

/** A monad to keep  track of an environment */
type EnvT[V, M[_]] = [A] =>> Kleisli[M, Environment[ValAddress[V]], A]

given [V: Lattice, M[_]: Monad]: EnvironmentM[EnvT[V, M], V] with {
    def getEnv: EnvT[V, M][Environment[ValAddress[V]]] =
        Kleisli(env => env.pure[M])
    def withEnv[X](f: Env => Environment[ValAddress[V]])(blk: EnvT[V, M][X]): EnvT[V, M][X] =
        Kleisli(env => blk.run(f(env)))
}

/** A monad to keep track of a context */
type CtxT[Ctx, M[_]] = [A] =>> Kleisli[M, Ctx, A]

given [Ctx0, M[_]: Monad]: CtxM[CtxT[Ctx0, M]] with {
    override type Ctx = Ctx0

    def getCtx: CtxT[Ctx0, M][Ctx] =
        Kleisli(env => env.pure[M])
    def withCtx[X](f: Ctx => Ctx)(blk: CtxT[Ctx0, M][X]): CtxT[Ctx0, M][X] =
        Kleisli(env => blk.run(f(env)))
}

//
// Allocation
//

// TODO

//
// ModF monad & algorithm
//

/** A monad implementing this typeclass supports operations for fetching effects, the global store and a set of seen components */
trait ModfM[M[_]](using val analysisM: CtxM[M]):
    import analysisM.*

    /** Returns a set of components seen in previous runs of the algorithm */
    def seen[X](m: M[X]): Set[Component[Ctx]]

    /** Returns the read effects of the intra-analysis represented by this monad */
    def reads[X](m: M[X]): Set[Effect]

    /** Returns the write effects of the intra-analysis represented by this monad */
    def writes[X](m: M[X]): Set[Effect]

    /** Returns the call effects of the intra-analysis represented by this monad */
    def calls[X](m: M[X]): Set[Component[Ctx]]

//
// Putting it all together
//

//
// Glue code for Monad transformers
//
