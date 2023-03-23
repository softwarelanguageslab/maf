package maf.values
package typeclasses

/** Typeclass for representing Galois connections between a concrete and an
  * abstract value.
  *
  * @tparam C
  *   the type of the concrete value
  * @tparam A
  *   the type of the abstract value
  */
trait Galois[C, A]:
  /** Inject a concrete value into the abstract domain, this function
    * corresponds to the `alpha` function in a Galois connection
    */
  def inject(c: C): A

object Galois:
  /** Injects a concrete value into the abstract domain using the Galois
    * connection in context
    */
  def inject[C, A](v: C)(using galois: Galois[C, A]): A =
    galois.inject(v)

  //
  // Trivial Galois implementations
  //

  given Galois[Unit, Unit] with
    def inject(c: Unit): Unit = ()

/** Convience type alias that curries the concrete and abstract type, used for
  * context bounds
  */
type GaloisFrom[C] = [A] =>> Galois[C, A]
