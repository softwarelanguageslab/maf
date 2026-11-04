package maf.util

trait MonadError[M[_], E] extends Monad[M]:
    def fail[X](err: E): M[X]

object MonadError:
    def apply[M[_], E](using MonadError[M, E]): MonadError[M, E] =
        summon[MonadError[M, E]]
