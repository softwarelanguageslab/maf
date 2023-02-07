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
    case class Call(lam: Lam, ctx: Ctx) extends Component
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
        sto: Sto,
        W: Set[Dependency],
        R: Set[Dependency],
        C: Set[Component])

    // EffectT monad

    case class EffectT_[M[_], A](runStore: Effects => M[Either[Effects, (Effects, A)]])
    type EffectT[M[_]] = [A] =>> EffectT_[M, A]

    private def modify[M[_]: Monad](f: Effects => Effects): EffectT[M][Unit] =
        EffectT_(e => Monad[M].unit(Right((f(e), ()))))

    private def spawn[M[_]: Monad](cmp: Component): EffectT[M][Unit] =
        modify(e => e.copy(C = e.C + cmp))

    private def read[M[_]: Monad](adr: Address): EffectT[M][Value] =
        modify(e => e.copy(R = e.R + AddrDependency(adr))) >>> get.map(_.sto.lookup(adr).getOrElse(lattice.bottom))

    private def get[M[_]: Monad]: EffectT[M][Effects] =
        EffectT_(e => Monad[M].unit(Right((e, e))))

    protected given [M[_]: Monad]: Monad[EffectT[M]] with
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

    protected given [M[_]: Monad]: MonadLift[EffectT_] with
        def lift[M[_]: Monad, A](m: M[A]): EffectT_[M, A] =
            EffectT_(e => m.map(v => Right((e, v))))

    protected given [M[_]: Monad]: MonadUnlift[EffectT_] with
        def unlift[M[_]: Monad, A, B](t: EffectT_[M, A])(f: M[A] => EffectT_[M, B]): EffectT_[M, B] =
            EffectT_(e =>
                t.runStore(e).flatMap {
                    case Left(e2)       => Monad[M].unit(Left(e2))
                    case Right((e2, v)) => f(Monad[M].unit(v)).runStore(e2)
                }
            )

    protected given [M[_]: AnalysisM]: AnalysisM[EffectT[M]] with
        private type A[X] = EffectT[M][X]
        private val stub: AnalysisM[EffectT[M]] = stubAnalysisM
        export stub.{call => _, *}

        /**
         * Reads the context and spawns a new component for <code>lam</code>.
         *
         * @note
         *   assumes that the argument values are already written to the corresponding addresses
         */
        def call(lam: Lam): A[Value] =
            for
                ctx <- getCtx
                cmp = Call(lam, ctx)
                _ <- spawn(cmp)
                vlu <- read(ReturnAddr(cmp, lam.idn))
            yield vlu
