package maf.modular.scheme.monadic

import maf.core.Monad
import maf.core.Monad.*

trait MonadFix_[M[_]: Monad, F]:
    def init: M[F]
    def step(in: F): M[F]
    def hasChanged(prev: F, next: F): M[Boolean] =
        Monad[M].unit(prev != next)

type MonadFix[F] = [M[_]] =>> MonadFix_[M, F]

object MonadFix:
    /** Runs the monad until a fix point is reached */
    def fix[M[_]: Monad, F, X](using m: MonadFix[F][M]): M[F] =
        def loop(prev: F, current: F): M[F] =
            m.hasChanged(prev, current).flatMap { changed =>
                if changed then
                    for
                        next <- m.step(current)
                        result <- loop(current, next)
                    yield result
                else Monad[M].unit(current)
            }

        m.init.flatMap((init) => m.step(init).flatMap(loop(init, _)))
