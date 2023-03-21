package maf.values
package typeclasses

import maf.util.*

/** An abstract operation on argument `A` with return type `R`
  *
  * Examples:
  * @example
  *   case object Plus extends Op[Int :: Int :: HNil, Int]
  */
trait Op[A <: HList, R]
