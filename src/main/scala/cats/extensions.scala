package cats
package extensions

type MonadError[E] = [M[_]] =>> cats.MonadError[M, E]
