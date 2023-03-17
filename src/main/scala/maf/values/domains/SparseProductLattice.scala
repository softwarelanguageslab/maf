package maf
package values
package domains

import typeclasses.*
import util.*

/** Represents a key of a type in the lattice */
trait LatKey:
  /** The concrete value of the key */
  type Concrete

  /** The abstract value of the key */
  type Abstract

  /** A Galois connection such that concrete values can be injected in the
    * abstract domain, and abstract values extracted
    */
  def galois: Galois[Concrete, Abstract]

  /** Support for primitive lattice operations on the abstract types */
  def lat: Lattice[Abstract]

object LatKey:
  /** Auxilary alias for a lattice key, can be used to fill in the abstract type
    * members using type parameters
    */
  type Aux[C, A] = LatKey { type Concrete = C; type Abstract = A }

  /** Convience constructor for a LatKey given a lattice for the abstract value
    * and a galois connection between the abstract and concrete value
    */
  def T[C, A: Lattice](using Galois[C, A]): LatKey =
    new LatKey:
      type Concrete = C
      type Abstract = A
      val galois = summon
      val lat = summon

  /** Any mapping between a LatKey and its corresponding abstract value is a
    * valid mapping. This given provides evidence of this fact automatically.
    */
  given [C, A]: Mapping[LatKey.Aux[C, A], A] = Mapping.derived

/** A product lattice represented as an `HMap` to limit memory usuage when its
  * contents are sparse.
  */
abstract class SparseProductLattice[K <: LatKey] extends Lattice[HMap[K]]:

  def bottom: HMap[K] = HMap.empty
  override def isBottom(x: HMap[K]): Boolean = x.isEmpty
  // top is undefined
  def top = throw LatticeTopUndefined

  def join(x: HMap[K], y: => HMap[K]): HMap[K] =
    // joins the corresponding keys in the HMap together
    (x.keysWithEvidence ++ y.keysWithEvidence).foldLeft(x) {
      case (result, (key, ev)) =>
        result.put(
          key,
          key.lat.join(
            result.get(key)(using ev).getOrElse(key.lat.bottom),
            y.get(key)(using ev).getOrElse(key.lat.bottom)
          )
        )(using ev)
    }

  def subsumes(x: HMap[K], y: HMap[K]): Boolean =
    // ∀k ∈ domain(x) ∪ domain(y):  y(k) ⊑ x(k)
    (x.keysWithEvidence ++ y.keysWithEvidence).forall { case (key, ev) =>
      key.lat.subsumes(
        x.get(key)(using ev).getOrElse(key.lat.bottom),
        y.get(key)(using ev).getOrElse(key.lat.bottom)
      )
    }

  def eql[B: BoolLattice](x: HMap[K], y: HMap[K]): B = ???
