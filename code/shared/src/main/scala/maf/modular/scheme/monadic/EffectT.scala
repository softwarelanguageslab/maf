package maf.modular.scheme.monadic

import maf.core.Monad
import maf.core.Monad.*
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.*
import maf.core.BasicStore
import maf.core.Address
import maf.modular.Dependency
import maf.core.monad.MonadLift
import maf.core.monad.MonadUnlift
import maf.modular.AddrDependency
import maf.modular.ReturnAddr
import maf.core.Identifier
import maf.core.MaybeEq
import maf.lattice.interfaces.BoolLattice
import maf.core.worklist.FIFOWorkList

/** Adds a ModF interpretation */
trait EffectT extends SchemeSemantics, StubAnalysisM:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    //
    // Type aliases (for convience)
    //

    type Sto = BasicStore[Address, Value]

    //
    // Components
    //

    sealed trait Component
    case class Call(lam: Lam, env: Env, ctx: Ctx) extends Component
    case object Main extends Component

    //
    // State
    //

    /**
     * Internal state to keep track of ModF effects
     *
     * @param sto
     *   the global store
     * @param W
     *   triggered dependencies (writes)
     * @param R
     *   dependencies (reads)
     * @param C
     *   spawns (calls)
     */
    case class Effects(
        cmp: Component,
        sto: Sto,
        wl: FIFOWorkList[Component] = FIFOWorkList.empty,
        seen: Set[Component] = Set(),
        W: Set[Dependency] = Set(),
        R: Map[Dependency, Set[Component]] = Map(),
        C: Set[Component] = Set())

    // EffectT monad

    case class EffectT_[M[_], A](runStore: Effects => M[Either[Effects, (Effects, A)]])
    type EffectT[M[_]] = [A] =>> EffectT_[M, A]

    private def currentCmp[M[_]: Monad]: EffectT[M][Component] =
        EffectT_(e => Monad[M].unit(Right((e, e.cmp))))

    private def modify[M[_]: Monad](f: Effects => Effects): EffectT[M][Unit] =
        EffectT_(e => Monad[M].unit(Right((f(e), ()))))

    private def spawn[M[_]: Monad](cmp: Component): EffectT[M][Unit] =
        modify(e => e.copy(C = e.C + cmp))

    private def read[M[_]: Monad](adr: Address): EffectT[M][Value] =
        val dep = AddrDependency(adr)
        val r = (e: Effects) => (dep -> (e.R.get(dep).getOrElse(Set()) + e.cmp))
        modify(e => e.copy(R = e.R + r(e))) >>> get.map(_.sto.lookup(adr).getOrElse(lattice.bottom))

    private def write[M[_]: Monad](adr: Address, v: Value): EffectT[M][Unit] =
        modify(e => e.copy(W = e.W + AddrDependency(adr), sto = e.sto.extend(adr, v)))

    private def get[M[_]: Monad]: EffectT[M][Effects] =
        EffectT_(e => Monad[M].unit(Right((e, e))))

    protected given me[M[_]: Monad]: Monad[EffectT[M]] with
        type A[X] = EffectT[M][X]
        def unit[X](x: X): A[X] = EffectT_(e => Monad[M].unit(Right((e, x))))
        def flatMap[X, Y](m: EffectT[M][X])(f: X => EffectT[M][Y]): EffectT[M][Y] =
            EffectT_(e =>
                m.runStore(e).flatMap {
                    case Right((e2, a)) => f(a).runStore(e2)
                    case Left(e2)       => Monad[M].unit(Left(e2))
                }
            )
        def map[X, Y](m: EffectT[M][X])(f: X => Y): EffectT[M][Y] =
            flatMap(m)(f andThen unit)

    protected given ml[M[_]: Monad]: MonadLift[EffectT_] with
        def lift[M[_]: Monad, A](m: M[A]): EffectT_[M, A] =
            EffectT_(e => m.map(v => Right((e, v))))

    protected given ul[M[_]: Monad]: MonadUnlift[EffectT_] with
        def unlift[M[_]: Monad, A, B](t: EffectT_[M, A])(f: M[A] => EffectT_[M, B]): EffectT_[M, B] =
            EffectT_(e =>
                t.runStore(e).flatMap {
                    case Left(e2)       => Monad[M].unit(Left(e2))
                    case Right((e2, v)) => f(Monad[M].unit(v)).runStore(e2)
                }
            )

    protected given effectTAnl[M[_]](using next: AnalysisM[M]): AnalysisM[EffectT[M]] with
        private type A[X] = EffectT[M][X]
        private val stub: AnalysisM[EffectT[M]] = stubAnalysisM[EffectT_, M](using ml, ul, next, me)
        export stub.{addrEq => _, call => _, extendSto => _, lookupSto => _, updateSto => _, *}

        /**
         * Reads the context and spawns a new component for <code>lam</code>.
         *
         * @note
         *   assumes that the argument values are already written to the corresponding addresses
         */
        def call(lam: Lam): A[Value] =
            for
                ctx <- getCtx
                env <- getEnv
                cmp = Call(lam, env, ctx)
                _ <- spawn(cmp)
                vlu <- read(ReturnAddr(cmp, lam.idn))
            yield vlu

        /** Looks up the given address in the global store */
        def lookupSto(a: Adr): A[Value] =
            read(a)

        /** Extend the global store with the given value */
        def extendSto(a: Adr, v: Value): A[Unit] =
            write(a, v)

        /** Update the store with the given value */
        def updateSto(a: Adr, v: Value): A[Unit] = extendSto(a, v)

        /** Pointer equality */
        def addrEq: A[MaybeEq[Adr]] = unit(new MaybeEq[Adr] {
            def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
                if a1 == a2 then BoolLattice[B].top else BoolLattice[B].inject(false)
        })
