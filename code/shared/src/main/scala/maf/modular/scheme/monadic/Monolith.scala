package maf.modular.scheme.monadic

import maf.core.*
import maf.core.Monad.*
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.modular.scheme.SchemeDomain
import maf.core.worklist.FIFOWorkList
import maf.modular.Dependency
import maf.modular.AddrDependency
import maf.modular.ReturnAddr
import maf.lattice.interfaces.BoolLattice
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeBegin

trait Monolith extends SchemeSemantics:
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
        C: Set[Component] = Set()):
        def merge(other: Effects): Effects =
            this.copy(sto = this.sto.extend(other.sto.content.toIterable), W = this.W ++ other.W, R = this.R ++ other.R, C = this.C ++ other.C)

    // EffectT monad

    case class EffectT_[M[_], A](runStore: (Env, Ctx, Effects) => M[(Effects, Option[A])])
    type EffectT[M[_]] = [A] =>> EffectT_[M, A]

    private def currentCmp[M[_]: Monad]: EffectT[M][Component] =
        EffectT_((_, _, e) => Monad[M].unit((e, Some(e.cmp))))

    private def modify[M[_]: Monad](f: Effects => Effects): EffectT[M][Unit] =
        EffectT_((_, _, e) => Monad[M].unit((f(e), Some(()))))

    private def spawn[M[_]: Monad](cmp: Component): EffectT[M][Unit] =
        modify(e => e.copy(C = e.C + cmp))

    private def read[M[_]: Monad](adr: Address): EffectT[M][Value] =
        val dep = AddrDependency(adr)
        val r = (e: Effects) => (dep -> (e.R.get(dep).getOrElse(Set()) + e.cmp))
        modify(e => e.copy(R = e.R + r(e))) >>> get.map(_.sto.lookup(adr).getOrElse(lattice.bottom))

    private def write[M[_]: Monad](adr: Address, v: Value): EffectT[M][Unit] =
        modify(e =>
            if e.sto.lookup(adr).getOrElse(lattice.bottom) == v then e
            else e.copy(W = e.W + AddrDependency(adr), sto = e.sto.extend(adr, v))
        )

    private def get[M[_]: Monad]: EffectT[M][Effects] =
        EffectT_((_, _, e) => Monad[M].unit((e, Some(e))))

    protected given me[M[_]: Monad]: Monad[EffectT[M]] with
        type A[X] = EffectT[M][X]
        def unit[X](x: X): A[X] = EffectT_((_, _, e) => Monad[M].unit((e, Some(x))))
        def flatMap[X, Y](m: EffectT[M][X])(f: X => EffectT[M][Y]): EffectT[M][Y] =
            EffectT_((env, ctx, e) =>
                m.runStore(env, ctx, e).flatMap {
                    case (e2, Some(a)) => f(a).runStore(env, ctx, e2)
                    case (e2, None)    => Monad[M].unit((e2, None))
                }
            )
        def map[X, Y](m: EffectT[M][X])(f: X => Y): EffectT[M][Y] =
            flatMap(m)(f andThen unit)

    protected given effectTAnl[M[_]: Monad]: AnalysisM[EffectT[M]] with
        private type A[X] = EffectT[M][X]
        private val mon: Monad[EffectT[M]] = me
        export mon.*

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

        override def mbottom[X]: A[X] = EffectT_((_, _, e) => Monad[M].unit(e, None))
        override def mjoin[X: Lattice](x: EffectT[M][X], y: EffectT[M][X]): EffectT[M][X] = EffectT_((env, ctx, eff) =>
            x.runStore(env, ctx, eff)
                .flatMap(x =>
                    y.runStore(env, ctx, eff)
                        .map(y =>
                            (x._1.merge(y._1),
                             (x._2, y._2) match
                                 case (None, None)       => None
                                 case (Some(x), None)    => Some(x)
                                 case (None, Some(y))    => Some(y)
                                 case (Some(x), Some(y)) => Some(Lattice[X].join(x, y))
                            )
                        )
                )
        )

        override def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            EffectT_((e, ctx, eff) => blk.runStore(f(e), ctx, eff))
        override def getEnv: A[Env] =
            EffectT_((e, _, eff) => Monad[M].unit((eff, Some(e))))
        override def getCtx: A[Ctx] =
            EffectT_((e, ctx, eff) => Monad[M].unit((eff, Some(ctx))))
        override def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            EffectT_((e, ctx, eff) => blk.runStore(e, f(ctx), eff))
        override def fail[X](err: Error): EffectT[M][X] = {
            println(err); mbottom
        }
