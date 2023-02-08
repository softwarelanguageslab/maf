package maf.modular.scheme.monadic

trait MonadFix_[M[_], F]:
    def init: F
    def step(in: F): F
    def hasChanged(prev: F, next: F): Boolean =
        prev != next

type MonadFix[F] = [M[_]] =>> MonadFix_[M, F]

object MonadFix:
    /** Runs the monad until a fix point is reached */
    def fix[M[_], F, X](using m: MonadFix[F][M]): F =
        var prev = m.init
        var current = m.step(prev)
        while (m.hasChanged(prev, current)) do
            val tmp = prev
            prev = current
            current = m.step(tmp)

        current
