package maf.modular.scheme.monadic

import maf.modular.scheme.*
import maf.modular.scheme.modflocal.*
import maf.core.IdentityMonad
import maf.core.IdentityMonad.Id
import maf.core
import maf.core.Monad
import maf.core.Lattice
import maf.core.MaybeEq
import maf.core.monad.{MonadLift, MonadUnlift}

trait IdentityAnalysisM extends SchemeSemantics:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    type Id[X] = IdentityMonad.Id[X]

    private def bottomOfStack[A](method: String): A =
        sys.error(s"bottom of monad transformer stack, no implementation for $method found")

    given AnalysisM[IdentityMonad.Id] with
        override def call(lam: Lam): Id[Val] = bottomOfStack("call")
        override def mbottom[X]: Id[X] = bottomOfStack("mbottom")
        override def getCtx: Id[Ctx] = bottomOfStack("getCtx")
        override def extendSto(a: Adr, v: Val): Id[Unit] = bottomOfStack("extendSto")
        override def withEnv[X](f: Env => Env)(blk: Id[X]): Id[X] = bottomOfStack("withEnv")
        override def withCtx[X](ctx: Ctx => Ctx)(blk: Id[X]): Id[X] = bottomOfStack("withCtx")
        override def lookupSto(a: Adr): Id[Val] = bottomOfStack("lookupSto")
        override def updateSto(a: Adr, v: Val): Id[Unit] = bottomOfStack("updateSto")
        override def getEnv: Id[Env] = bottomOfStack("getEnv")
        override def addrEq: Id[MaybeEq[Adr]] = bottomOfStack("addrEq")
        override def mjoin[X: Lattice](x: Id[X], y: Id[X]): Id[X] = bottomOfStack("mjoin")
        override def fail[X](err: core.Error): Id[X] = bottomOfStack("fail")

        export IdentityMonad.idMonad.*

trait StubAnalysisM extends SchemeSemantics:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    protected def stubAnalysisM[T[_[_], _]: MonadLift: MonadUnlift, M[_]: AnalysisM](using monad: Monad[[A] =>> T[M, A]]) =
        new AnalysisM[[A] =>> T[M, A]] {
            import MonadLift.*
            private val next: AnalysisM[M] = summon[AnalysisM[M]]
            export monad.*
            override def call(lam: Lam): T[M, Val] =
                lift(next.call(lam))
            override def mbottom[X]: T[M, X] =
                lift(next.mbottom)
            override def getCtx: T[M, Ctx] =
                lift(next.getCtx)
            override def withEnv[X](f: Env => Env)(blk: T[M, X]): T[M, X] =
                unlift(blk) { blk => lift(next.withEnv(f)(blk)) }
            override def withCtx[X](ctx: Ctx => Ctx)(blk: T[M, X]): T[M, X] =
                unlift(blk) { blk => lift(next.withCtx(ctx)(blk)) }
            override def extendSto(a: Adr, v: Val): T[M, Unit] =
                lift(next.extendSto(a, v))
            override def lookupSto(a: Adr): T[M, Val] =
                lift(next.lookupSto(a))
            override def updateSto(a: Adr, v: Val): T[M, Unit] =
                lift(next.updateSto(a, v))
            override def getEnv: T[M, Env] =
                lift(next.getEnv)
            override def addrEq: T[M, MaybeEq[Adr]] =
                lift(next.addrEq)
            override def mjoin[X: Lattice](x: T[M, X], y: T[M, X]): T[M, X] =
                unlift(x)(x =>
                    unlift(y) { y =>
                        lift(next.mjoin(x, y))
                    }
                )
            override def fail[X](err: core.Error): T[M, X] =
                lift(next.fail(err))
        }
