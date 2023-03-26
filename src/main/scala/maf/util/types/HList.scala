package maf.util.types

import scala.annotation.implicitNotFound
import scala.annotation.implicitAmbiguous

/** A typelevel heterogenous list
  */
trait HList

/** Nil type, used at the end of the list */
object HNil extends HList
type HNil = HNil.type

/** A constructor for pairs consisting of an arbitrary head and a tail that is
  * also an HList
  */
class ::[H, T <: HList] extends HList

/** Predicate that succeeds when the list does not contain the given element.
  *
  * @note
  *   This is implemented using "negation as failure", if the element `does`
  *   appear in the list the Scala compiler will fail to find an unambious
  *   implicit resulting in a failure to construct evidence that the list does
  *   not contain the given element
  */
trait ContainsNot[A, L <: HList]

// NOTE: `implicit def` is used here despite it being phased out from Scala 3 in favor of `given`  and `using`,
// however error reporting using @implicitAmbiguous does not function correctly when used in combination with `given`.
@implicitAmbiguous("Could not proof that ${A} ∉ ${A} :: ${L}")
implicit def doesContain1[A, L <: HList]: ContainsNot[A, A :: L] =
  new ContainsNot {}
implicit def doesContain2[A, L <: HList]: ContainsNot[A, A :: L] =
  new ContainsNot {}

/** If the head of the list does not contain the given element, then we also
  * need proof that its tail does not
  */
given [A, H, R <: HList](using ContainsNot[A, R]): ContainsNot[A, H :: R]
  with {}

/** The empty list does not contain the given element */
given [A]: ContainsNot[A, HNil] with {}
