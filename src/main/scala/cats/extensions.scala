package cats
package extensions

import cats.*
import cats.syntax.all.*
import maf.util.*

type MonadError[E] = [M[_]] =>> cats.MonadError[M, E]

object Errors {
  export ApplicativeError.liftFromOption

  def raiseError[M[_]: MonadError[Error], A](msg: String): M[A] =
    ApplicativeError[M, Error].raiseError(StringError(msg))

  def raiseError[M[_]: MonadError[Error], A](e: Error): M[A] =
    ApplicativeError[M, Error].raiseError(e)

}

extension [A](vs: List[A])
  def foldRightM[M[_]: Monad, B](z: B)(fa: (A, B) => M[B]): M[B] =
    vs match
      case Nil     => z.pure
      case x :: xs => xs.foldRightM(z)(fa) >>= (rst => fa(x, rst))

extension [M[_]: Monad, A, B, C](v: (M[A], M[B]))
  def flatMapN(f: (A, B) => M[C]): M[C] =
    for
      av <- v._1
      bv <- v._2
      cv <- f(av, bv)
    yield cv
