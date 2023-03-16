package cats
package extensions

import cats.*

type MonadError[E] = [M[_]] =>> cats.MonadError[M, E]

object Errors {
  export ApplicativeError.liftFromOption

}
