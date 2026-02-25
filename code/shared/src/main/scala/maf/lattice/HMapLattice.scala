package maf.lattice

import maf.core.*
import maf.util.{Default, Show}
import maf.lattice.interfaces.BoolLattice

type NoExtract = Nothing
type NoInject = Nothing

trait HMapKey:
    type Extract
    type Inject
    type Abstract
    type Wrap

    val lattice: Lattice[Abstract]

    def inject(v: Inject): Abstract = throw new Exception("injection not possible")
    def extract(v: Abstract): Extract = throw new Exception("extraction not possible")
    def unwrap(v: Wrap): Abstract
    def wrap: (Abstract => Wrap)
    def alpha(v: Inject): Wrap = wrap(inject(v))

trait AbstractType[T, W] extends HMapKey:
    type Extract
    type Inject
    type Abstract = T
    type Wrap = W

trait AbstractSetType[T, W <: Product1[Set[T]]] extends AbstractType[Set[T], W]:
    type Extract = Set[T]
    type Inject = T
    override def inject(v: Inject): Abstract = Set(v)
    override def extract(v: Abstract): Extract = v
    def unwrap(v: Wrap): Abstract = v._1
    given Show[T] with
        def show(v: T): String = v.toString
    val lattice: Lattice[Abstract] = Lattice.setLattice

trait AbstractWrapType[T: Lattice, W <: Product1[T]] extends AbstractType[T, W]:
    type Extract = NoExtract
    val lattice: Lattice[Abstract] = summon[Lattice[T]]
    def unwrap(v: Wrap): Abstract = v._1

case class HMap(contents: Map[HMapKey, Any]):
    /** Inject the given concrete value in the abstract domain */
    def inject(k: HMapKey, v: k.Inject): HMap =
        val abstr = k.inject(v)
        val updated: k.Wrap =
            contents.get(k).map(_.asInstanceOf[k.Wrap]).map(orig => k.wrap(k.lattice.join(k.unwrap(orig), abstr))).getOrElse(k.wrap(abstr))
        this.copy(contents = contents + (k -> updated))

    def wrapInsert(k: HMapKey, v: k.Wrap): HMap =
        val updated: k.Wrap = contents.get(k).map(_.asInstanceOf[k.Wrap]).map(orig => k.wrap(k.lattice.join(k.unwrap(orig), k.unwrap(v)))).getOrElse(v)
        this.copy(contents = contents + (k -> updated))

    /** Get a value (if any) of the given type */
    def get(k: HMapKey)(using default: Default[k.Extract]): k.Extract =
        this.contents.get(k).map(_.asInstanceOf[k.Wrap]).map(k.unwrap andThen k.extract).getOrElse(default.default)

    def getAbstract(k: HMapKey): Option[k.Abstract] =
        this.contents.get(k).map(_.asInstanceOf[k.Wrap]).map(k.unwrap)

    /** Returns true if the HMap does not contain any value */
    def isEmpty: Boolean = contents.isEmpty

    /** Joins `this` with the given `HMap` */
    def join(y: HMap): HMap =
        val x = this
        HMap(
          (y.contents.keys).foldLeft(x.contents)((map, key) =>
              y.contents.get(key) match
                  case Some(v) =>
                      val newVlu = map.get(key).map(_.asInstanceOf[key.Wrap]).map(key.unwrap) match
                          case Some(otherVlu) =>
                              key.wrap(key.lattice.join(otherVlu, key.unwrap(v.asInstanceOf[key.Wrap])))
                          case None => v
                      map + (key -> newVlu)
                  case _ => map
          )
        )

    /** Returns all the keys in the HMap */
    def keys: Iterable[HMapKey] = this.contents.keys

    /** Checks subsumptions of `this` with y */
    def subsumes(y: HMap): Boolean =
        val x = this
        // for every element in `y` there exists an element in `x` of the same type that subsumes it
        y.contents.keys.forall((key) =>
            (for
                xv <- x.getAbstract(key)
                yv <- y.getAbstract(key)
            yield key.lattice.subsumes(xv, yv)).getOrElse(false)
        )

    /**
     * To provide a similar interface as the old Elements representation in MpdularSchemeLattice
     *
     * @note
     *   this is potentially unsafe if the HMap does not contain Wrappers of type W
     */
    def elements[W]: List[W] =
        this.contents.values.map(_.asInstanceOf[W]).toList

    def mapValue(key: HMapKey)(f: key.Abstract => key.Abstract): HMap =
        // changes the value in the hmap of the given key according to the given mapping function.
        // if there is no value of the given type, the mapping function is not applied.
        this.contents.get(key).map(_.asInstanceOf[key.Wrap]).map(key.unwrap).map(f) match
            case Some(newVlu) => this.copy(contents = this.contents + (key -> key.wrap(newVlu)))
            case None         => this

    override def toString: String =
        if contents.isEmpty then "⊥" else s"{${contents.values.map(_.toString).mkString(",")}}"

object HMap:
    /**
     * Creates a new HMap that contains one abstract value of the given type
     *
     * @param k
     *   the type of the abstract value
     * @param v
     *   the abstract value itself
     */
    def inserted(k: HMapKey, v: k.Abstract): HMap =
        HMap(contents = List(k -> k.wrap(v)).toMap)

    def injected(k: HMapKey, v: k.Inject): HMap =
        HMap.empty.inject(k, v)

    /** Same as `inserted` but only accepts wrapped types */
    def wrapInserted(k: HMapKey, v: k.Wrap): HMap =
        HMap(contents = List(k -> v).toMap)

    /** Creates an empty HMap */
    def empty: HMap = HMap(contents = Map())

    given Lattice[HMap] with
        def show(v: HMap): String = v.toString // TODO: improve
        def bottom: HMap = HMap.empty
        def top: HMap = throw LatticeTopUndefined
        override def isBottom(x: HMap): Boolean = x.isEmpty
        def join(x: HMap, y: => HMap): HMap = x.join(y)
        def subsumes(x: HMap, y: => HMap): Boolean = x.subsumes(y)
        def eql[B: BoolLattice](x: HMap, y: HMap): B = ???
        def level(x: HMap): scala.Int = x.keys.map(key => x.getAbstract(key).map(key.lattice.level(_)).foldLeft(1)(_ * _)).foldLeft(1)(_ * _)
