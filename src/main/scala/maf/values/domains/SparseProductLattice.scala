package maf
package values
package domains

import typeclasses.*
import util.*

/** Represents a key of a type in the lattice */
trait LatKey extends HMapKey:
  /** The concrete value of the key */
  type Concrete

  /** The abstract value of the key */
  type Abstract

  /** Value correzsponding to the key in the HMap */
  type Value = Abstract

  /** A Galois connection such that concrete values can be injected in the
    * abstract domain, and abstract values extracted
    */
  def galois: Galois[Concrete, Abstract]

  /** Support for primitive lattice operations on the abstract types */
  def lat: Lattice[Abstract]

  /** A human-readable name for the type, to be used for printing */
  val name: String

object LatKey:
  trait T[C, A: GaloisFrom[C]: Lattice] extends LatKey:
    type Concrete = C
    type Abstract = A
    def galois = summon
    def lat = summon

/** A product lattice represented as an `HMap` to limit memory usuage when its
  * contents are sparse.
  */
trait SparseProductLattice[K <: LatKey] extends Lattice[HMap[K]]:

  def bottom: HMap[K] = HMap.empty
  override def isBottom(x: HMap[K]): Boolean = x.isEmpty
  // top is undefined
  def top = throw LatticeTopUndefined

  def join(x: HMap[K], y: => HMap[K]): HMap[K] =
    // joins the corresponding keys in the HMap together
    (x.keys ++ y.keys).foldLeft(x) { case (result, key) =>
      result.put(
        key,
        key.lat.join(
          result.get(key).getOrElse(key.lat.bottom),
          y.get(key).getOrElse(key.lat.bottom)
        )
      )
    }

  def subsumes(x: HMap[K], y: => HMap[K]): Boolean =
    // ∀k ∈ domain(x) ∪ domain(y):  y(k) ⊑ x(k)
    (x.keys ++ y.keys).forall(key =>
      key.lat.subsumes(
        x.get(key).getOrElse(key.lat.bottom),
        y.get(key).getOrElse(key.lat.bottom)
      )
    )

  // TODO: rename to insert
  def inject(tpy: K, v: tpy.Concrete): HMap[K] =
    HMap.empty.put(tpy, tpy.galois.inject(v))

  def eql[B: BoolLattice: GaloisFrom[Boolean]](x: HMap[K], y: HMap[K]): B = ???

  //
  // Show
  //

  override def show(t: HMap[K]): String =
    t.keys
      .map { case (key) =>
        key.lat.show(t.get(key).getOrElse(key.lat.bottom))
      }
      .mkString("{", ",", "}")
