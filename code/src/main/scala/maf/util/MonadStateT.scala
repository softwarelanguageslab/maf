package maf.util

trait StateOps[S, M[_]] extends Monad[M]:
    given self: Monad[M] = this
    def get: M[S]
    def put(snew: S): M[Unit]

class MonadStateT[S, M[_]: Monad, A](val run: S => M[(A, S)]):
    def runValue(init: S) = Monad[M].map(run(init))(_._1)

object MonadStateT:
    def apply[S, M[_]: Monad, A](run: S => M[(A, S)]): MonadStateT[S, M, A] =
        new MonadStateT(run)

    given stateInstance[S, M[_]: Monad]: StateOps[S, [A] =>> MonadStateT[S, M, A]] with
        private type SM[A] = MonadStateT[S, M, A]
        def unit[X](x: X): SM[X] =
            MonadStateT((s: S) => Monad[M].unit((x, s)))
        def map[X, Y](m: SM[X])(f: X => Y): SM[Y] =
            MonadStateT((s: S) =>
                Monad[M].map(m.run(s)) { case (v, snew) =>
                    (f(v), snew)
                }
            )

        def flatMap[X, Y](m: SM[X])(f: X => SM[Y]): SM[Y] =
            MonadStateT((s: S) =>
                Monad[M].flatMap(m.run(s)) { case (v, snew) =>
                    f(v).run(snew)
                }
            )

        def get: SM[S] = MonadStateT((s: S) => Monad[M].unit((s, s)))
        def put(snew: S): SM[Unit] = MonadStateT((s: S) => Monad[M].unit(((), snew)))
