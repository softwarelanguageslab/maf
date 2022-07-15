// A proposal of how the ModularSchemeLattice can be improved in terms of code size based on the idea of HMap's.
// - The methods in the SchemeLattice are reduced to `get` and `inject` instead of seperate injectors and getters for each of the supported types
// - the size of the `join` is significantly reduced by requiring that each value that is used in the product lattice produced by the ModularSchemeLattice supports `Lattice` operations.
// - I added `inject` to the definition of the lattice, as well as a notion of a "concrete type" of which the lattice is an abstract representation of.

case class SchemeExp()
case class Environment()
case class Actor()

trait Lattice[L, C]:
    def bottom: L
    def inject(x: C): L
    def join(x: L, y: => L): L
    def subsumes(x: L, y: => L): Boolean

trait SchemeLattice[L] extends Lattice[L, Nothing]:
    def inject(x: Nothing): L = throw new Exception(
      "SchemeLattice does not have a correspondig concrete value, use inject/2 to specify the type of value to inject instead."
    )
    def inject[T <: AbstractType](tpy: T, vlu: tpy.ConcreteValue): L
    def get[T <: ATypeSet[_]](tpy: T, value: L): Set[tpy.ConcreteValue]

object Lattice:
    def setLattice[A]: Lattice[Set[A], A] = new Lattice {
        def bottom: Set[A] = Set()
        def inject(x: A): Set[A] = Set(x)
        def join(x: Set[A], y: => Set[A]): Set[A] = x ++ y
        def subsumes(x: Set[A], y: => Set[A]): Boolean = y.subsetOf(x)
    }

trait IntLattice[I] extends Lattice[I, Int]

trait AbstractType { type AbstractValue; type ConcreteValue; given lat: Lattice[AbstractValue, ConcreteValue] }
trait AType[A, C](using l: Lattice[A, C]) extends AbstractType { type AbstractValue = A; type ConcreteValue = C; given lat: Lattice[A, C] = l }
trait ATypeSet[A] extends AbstractType:
    type AbstractValue = Set[A]
    type ConcreteValue = A
    given lat: Lattice[Set[A], A] = Lattice.setLattice

case class Value(contents: Map[AbstractType, Any])

class ModularSchemeLattice[I: IntLattice] extends SchemeLattice[Value]:
    object Clo extends ATypeSet[(SchemeExp, Environment)]
    object Act extends ATypeSet[Actor]
    object Num extends AType[I, Int]

    def bottom: Value = Value(Map())
    def inject[T <: AbstractType](tpy: T, vlu: tpy.ConcreteValue): Value =
        Value(Map(tpy -> tpy.lat.inject(vlu)))

    def get[T <: ATypeSet[_]](tpy: T, value: Value): Set[tpy.ConcreteValue] =
        value.contents.get(tpy).map(_.asInstanceOf[tpy.AbstractValue]).getOrElse(Set())

    def join(x: Value, y: => Value): Value = Value(
      (y.contents.keys).foldLeft(x.contents)((map, key) =>
          y.contents.get(key) match
              case Some(v) =>
                  val vlu =
                      key.lat.join(map.get(key).map(_.asInstanceOf[key.AbstractValue]).getOrElse(key.lat.bottom), v.asInstanceOf[key.AbstractValue])
                  map + (key -> vlu)
              case _ => map
      )
    )

    def subsumes(x: Value, y: => Value): Boolean = ???

enum CP[+T]:
    case Bottom
    case Constant(n: T)
    case Top

object CP:
    given cpLattice[T]: Lattice[CP[T], T] with
        type L = CP[T]
        def bottom: L = CP.Bottom
        def join(x: L, y: => L): L = (x, y) match
            case (CP.Bottom, y) => y
            case (x, CP.Bottom) => x
            case (_, _)         => CP.Top

        def subsumes(x: L, y: => L): Boolean = (x, y) match
            case (CP.Top, _) => true
            case (_, Bottom) => true
            case _           => false

        def inject(x: T): L = Constant(x)

    val exportIntCpLattice = cpLattice[Int]
    given intCpLattice: IntLattice[CP[Int]] with
        export exportIntCpLattice.*

@main def run(): Unit =
    val schemeLattice = new ModularSchemeLattice[CP[Int]]
    val v1 = schemeLattice.inject(schemeLattice.Num, 5)
    val v2 = schemeLattice.inject(schemeLattice.Num, 6)
    println(v1)
    println(v2)
    val v3 = schemeLattice.join(v1, v2)
    val v4 = schemeLattice.inject(schemeLattice.Clo, (SchemeExp(), Environment()))
    val v5 = schemeLattice.join(v3, v4)
    println(v5)
    println(schemeLattice.join(v1, v4))
    println(schemeLattice.get(schemeLattice.Clo, v5))
