package maf.domain.lattice

import maf.util.Monad

trait MonadJoin[M[_]] extends Monad[M]:
    def mbottom[X]: M[X]
    def mjoin[X: Lattice](x: M[X], y: M[X]): M[X]
    // for convenience
    def mjoin[X: Lattice](xs: Iterable[M[X]]): M[X] =
        xs.foldLeft(mbottom: M[X])((acc, m) => mjoin(acc, m))
    def mfold[X: Lattice](xs: Iterable[X]): M[X] =
        mjoin(xs.map(unit))
    def mfoldMap[X, Y: Lattice](xs: Iterable[X])(f: X => M[Y]): M[Y] =
        mjoin(xs.map(f))
    def guard(cnd: Boolean): M[Unit] =
        if cnd then unit(()) else mbottom
    def withFilter[X](m: M[X])(p: X => Boolean): M[X] =
        flatMap(m)(x => map(guard(p(x)))(_ => x))
    def inject[X: Lattice](x: X): M[X] =
        map(guard(!Lattice[X].isBottom(x)))(_ => x)

object MonadJoin:
    // to easily access an implicit Monad instance
    def apply[M[_]: MonadJoin]: MonadJoin[M] = implicitly
    // nicer syntax in Scala
    implicit class MonadJoinSyntaxOps[M[_]: MonadJoin, X](self: M[X]):
        def ++(other: M[X])(implicit ev: Lattice[X]): M[X] = MonadJoin[M].mjoin(self, other)
        def withFilter(p: X => Boolean): M[X] = MonadJoin[M].withFilter(self)(p)
    implicit class MonadJoinIterableSyntax[X](xs: Iterable[X]):
        def foldMapM[M[_]: MonadJoin, Y: Lattice](f: X => M[Y]): M[Y] = MonadJoin[M].mfoldMap(xs)(f)
