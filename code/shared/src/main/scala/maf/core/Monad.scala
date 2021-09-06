package maf.core

//
// Monad
//

trait Monad[M[_]] {
  def unit[X](x: X): M[X]
  def map[X, Y](m: M[X])(f: X => Y): M[Y]
  def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y]
}

object Monad {
  // to easily access an implicit Monad instance
  def apply[M[_]: Monad]: Monad[M] = implicitly
  // necessary to get the fancy for-yield syntax in Scala
  implicit class MonadSyntaxOps[M[_]: Monad, X](self: M[X]) {
    def map[Y](f: X => Y): M[Y] = Monad[M].map(self)(f)
    def flatMap[Y](f: X => M[Y]): M[Y] = Monad[M].flatMap(self)(f)
    def >>=[Y](f: X => M[Y]): M[Y] = flatMap(f)
    def >>>[Y](m: => M[Y]): M[Y] = flatMap(_ => m)
  }
  // some common monad operations on iterables
  implicit class MonadIterableOps[X](xs: Iterable[X]) {

    def mapM[M[_]: Monad, Y](f: X => M[Y]): M[List[Y]] =
      if xs.isEmpty then Monad[M].unit(Nil)
      else
        for {
          fx <- f(xs.head)
          rst <- xs.tail.mapM(f)
        } yield fx :: rst

    def mapM_[M[_]: Monad, Y](f: X => M[Y]): M[Unit] =
      if xs.isEmpty then Monad[M].unit(())
      else f(xs.head) >>> { xs.tail.mapM_(f) }

    def foldLeftM[M[_]: Monad, Y](acc: Y)(f: (Y, X) => M[Y]): M[Y] =
      if xs.isEmpty then Monad[M].unit(acc)
      else f(acc, xs.head) >>= { xs.tail.foldLeftM(_)(f) }

    def foldRightM[M[_]: Monad, Y](nil: Y)(f: (X, Y) => M[Y]): M[Y] =
      if xs.isEmpty then Monad[M].unit(nil)
      else xs.tail.foldRightM(nil)(f) >>= { f(xs.head, _) }
  }
}

//
// MonadError
//

trait MonadError[M[_], E] extends Monad[M] {
  def fail[X](err: E): M[X]
}

//
// MonadJoin
//

trait MonadJoin[M[_]] extends Monad[M] {
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
    if (cnd) unit(()) else mbottom
  def withFilter[X](m: M[X])(p: X => Boolean): M[X] =
    flatMap(m)(x => map(guard(p(x)))(_ => x))
  def inject[X: Lattice](x: X): M[X] =
    map(guard(!Lattice[X].isBottom(x)))(_ => x)
}

object MonadJoin {
  // to easily access an implicit Monad instance
  def apply[M[_]: MonadJoin]: MonadJoin[M] = implicitly
  // nicer syntax in Scala
  implicit class MonadJoinSyntaxOps[M[_]: MonadJoin, X](self: M[X]) {
    def ++(other: M[X])(implicit ev: Lattice[X]): M[X] = MonadJoin[M].mjoin(self, other)
    def withFilter(p: X => Boolean): M[X] = MonadJoin[M].withFilter(self)(p)
  }
  implicit class MonadJoinIterableSyntax[X](xs: Iterable[X]) {
    def foldMapM[M[_]: MonadJoin, Y: Lattice](f: X => M[Y]): M[Y] = MonadJoin[M].mfoldMap(xs)(f)
  }
}
