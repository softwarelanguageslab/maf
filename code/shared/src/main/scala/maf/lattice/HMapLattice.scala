package maf.lattice

import maf.core.*
import maf.lattice.interfaces.BoolLattice
import maf.util.Show

trait AbstractType:
    type AbstractValue
    type ConcreteValue
    val lat: Lattice[AbstractValue, ConcreteValue]

/**
 * Represents the type of an abstract type (that is an element of a lattice)
 *
 * @tparam A
 *   the abstract type of the given concrete type
 * @tparam C
 *   the concrete type corresponding to the given abstract type
 */
trait AType[A, C](using val lat: Lattice[A, C]) extends AbstractType:
    type AbstractValue = A
    type ConcreteValue = C

/** A type of an abstract type that is represented by the set lattice */
trait ATypeSet[C] extends AType[Set[C], C]

enum Singleton[+V]:
    case Bot
    case Single(v: V)

abstract class ATypeSingleton[V: Show](vlu: V) extends AbstractType:
    type AbstractValue = Singleton[V]
    type ConcreteValue = V

    val lat: Lattice[Singleton[V], V] = new Lattice:
        def show(x: Singleton[V]): String = x.toString
        def top = Singleton.Single(vlu)
        def bottom = Singleton.Bot
        def join(x: Singleton[V], y: => Singleton[V]): Singleton[V] = (x, y) match
            case (Singleton.Single(v), _) => Singleton.Single(v)
            case (_, Singleton.Single(v)) => Singleton.Single(v)
            case (_, _)                   => Singleton.Bot
        def subsumes(x: Singleton[V], y: => Singleton[V]): Boolean = (x, y) match
            case (Singleton.Bot, _) => false
            case (_, _)             => true
        def eql[B: BoolLattice](x: Singleton[V], y: Singleton[V]): B = ???

class HMap(private val contents: Map[AbstractType, Any]):
    def isEmpty: Boolean = contents.isEmpty
    def join(y: HMap): HMap =
        val x = this
        HMap(
          (y.contents.keys).foldLeft(x.contents)((map, key) =>
              y.contents.get(key) match
                  case Some(v) =>
                      val vlu =
                          key.lat.join(map.get(key).map(_.asInstanceOf[key.AbstractValue]).getOrElse(key.lat.bottom), v.asInstanceOf[key.AbstractValue])
                      map + (key -> vlu)
                  case _ => map
          )
        )

    def get[K <: AbstractType](k: K): Option[k.AbstractValue] =
        contents.get(k).map(_.asInstanceOf[k.AbstractValue])

    def keys: Iterable[AbstractType] = contents.keys

    def subsumes(y: HMap): Boolean =
        val x = this
        // for every element in `y` there exists an element in `x` of the same type that subsumes it
        y.contents.keys.forall((key) =>
            val xv = x.get(key).getOrElse(key.lat.bottom)
            val yv = y.get(key).getOrElse(key.lat.bottom)
            key.lat.subsumes(xv, yv)
        )

    /** Returns true if the HMap contain the given abstract type, and if the given abstract type is not bottom */
    def isSet[K <: AbstractType](tpy: K): Boolean =
        contents.get(tpy).map((vlu) => tpy.lat.isBottom(vlu.asInstanceOf[tpy.AbstractValue])).getOrElse(false)

    /** Retract the given value from the HMap */
    def retracted(k: AbstractType): HMap =
        HMap(this.contents - k)

object HMap:
    def empty: HMap = HMap(Map())

    def inject(k: AbstractType, v: k.ConcreteValue): HMap =
        HMap(Map(k -> k.lat.inject(v)))

    def inserted(k: AbstractType, v: k.AbstractValue): HMap =
        HMap(Map(k -> v))

    def top(k: AbstractType): HMap =
        inserted(k, k.lat.top)

    // No single concrete type of HMap, hence the "Nothing" in the lattice
    given lat: Lattice[HMap, Nothing] with
        def bottom: HMap = HMap.empty
        def isBottom(x: HMap): Boolean = x.isEmpty

        /**
         * Injection is not supported on the HMap, use the `inject` method of the HMap class that supports inserting a value according to a particular
         * key
         *
         * @see
         *   HMap.inject
         */
        def inject(c: Nothing): HMap = throw Exception("impossible program path")

        def top: HMap = throw LatticeTopUndefined
        def join(x: HMap, y: => HMap): HMap = x.join(y)
        def subsumes(x: HMap, y: => HMap): Boolean = x.subsumes(y)
        def eql[B: BoolLattice](x: HMap, y: => HMap): B = ???
