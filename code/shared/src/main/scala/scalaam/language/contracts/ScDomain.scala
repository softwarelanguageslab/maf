package scalaam.language.contracts

import scalaam.core.{Address, LatticeTopUndefined}
import scalaam.lattice.{BoolLattice, IntLattice}
import scalaam.util.SmartHash

trait ScDomain[I, B, Addr <: Address] {
  import ScLattice._

  protected[this] implicit def intLattice: IntLattice[I]
  protected[this] implicit def boolLattice: BoolLattice[B]

  sealed trait Value extends SmartHash {
    def ord: Int
  }

  case object TopValue extends Value {
    def ord                       = 0
    override def toString: String = "⊤"
  }

  case object BotValue extends Value {
    def ord               = 1
    override def toString = "⊥"
  }

  case class Bool(b: B) extends Value {
    def ord = 2
    override def toString: String = (Values.isTrue(this), Values.isFalse(this)) match {
      case (true, false)  => "true"
      case (false, true)  => "false"
      case (true, true)   => TopValue.toString
      case (false, false) => BotValue.toString
    }
  }

  case class Clos(clos: Set[Clo[Addr]]) extends Value {
    def ord = 3
  }

  case class Grds(grds: Set[Grd[Addr]]) extends Value {
    def ord = 4
  }

  case class Arrs(arrs: Set[Arr[Addr]]) extends Value {
    def ord = 5
  }

  case class Number(i: I) extends Value {
    def ord = 6
  }

  case class Opqs(opqs: Set[Opq]) extends Value {
    def ord = 7
  }

  case class Prims(prims: Set[Prim]) extends Value {
    def ord = 8
  }

  case class Blames(blames: Set[Blame]) extends Value {
    def ord = 9
  }

  /**
    * A symbolic expression
    */
  case class Symbolics(expr: Set[ScExp]) extends Value {
    def ord = 9
  }

  def bool(bool: Boolean): Value = Bool(BoolLattice[B].inject(bool))

  def number(n: Int): Value =
    Number(IntLattice[I].inject(n))

  def clo(clo: Clo[Addr]): Value =
    Clos(Set(clo))

  def grd(grd: Grd[Addr]): Value =
    Grds(Set(grd))

  def arr(arr: Arr[Addr]): Value =
    Arrs(Set(arr))

  def symbolic(expr: ScExp) =
    Symbolics(Set(expr))

  object Values {
    def join(a: Value, b: Value): Value = (a, b) match {
      case (TopValue, _) | (_, TopValue) => TopValue
      case (BotValue, _)                 => a
      case (_, BotValue)                 => b
      case (Number(a), Number(b))        => Number(IntLattice[I].join(a, b))
      case (Bool(a), Bool(b))            => Bool(BoolLattice[B].join(a, b))
      case (Clos(a), Clos(b))            => Clos(a ++ b)
      case (Arrs(a), Arrs(b))            => Arrs(a ++ b)
      case (Grds(a), Grds(b))            => Grds(a ++ b)
      case (Prims(a), Prims(b))          => Prims(a ++ b)
      case (Symbolics(a), Symbolics(b))  => Symbolics(a ++ b)
      case (Blames(a), Blames(b))        => Blames(a ++ b)
      case (_, _)                        => TopValue
    }

    def applyPrimitive(prim: Prim)(arguments: Value*): Value =
      (prim, arguments.toList) match {
        case (Prim("+"), List(Number(a), Number(b)))                       => Number(IntLattice[I].plus(a, b))
        case (Prim("+"), List(TopValue | Number(_), TopValue | Number(_))) => TopValue
        case (Prim("+"), _)                                                => BotValue

        case (Prim("-"), List(Number(a), Number(b)))                       => Number(IntLattice[I].minus(a, b))
        case (Prim("-"), List(TopValue | Number(_), TopValue | Number(_))) => TopValue
        case (Prim("-"), _)                                                => BotValue

        case (Prim("*"), List(Number(a), Number(b)))                       => Number(IntLattice[I].times(a, b))
        case (Prim("*"), List(TopValue | Number(_), TopValue | Number(_))) => TopValue
        case (Prim("*"), _)                                                => BotValue

        case (Prim("/"), List(Number(a), Number(b)))                       => Number(IntLattice[I].quotient(a, b))
        case (Prim("/"), List(TopValue | Number(_), TopValue | Number(_))) => TopValue
        case (Prim("/"), _)                                                => BotValue

        case (Prim("even?"), List(Number(a))) =>
          val mod2         = IntLattice[I].modulo(a, IntLattice[I].inject(2))
          val possiblyEven = IntLattice[I].subsumes(mod2, IntLattice[I].inject(0))
          val possiblyOdd  = IntLattice[I].subsumes(mod2, IntLattice[I].inject(1))
          (possiblyEven, possiblyOdd) match {
            case (true, true)   => TopValue
            case (true, false)  => Bool(BoolLattice[B].inject(true))
            case (false, true)  => Bool(BoolLattice[B].inject(false))
            case (false, false) => BotValue
          }

        case (Prim("even?"), List(Opqs(_) | TopValue)) => TopValue
        case (Prim("even?"), _)                        => BotValue

        case (Prim("odd?"), List(Number(b))) =>
          val result = applyPrimitive(Prim("even?"))(Number(b))
          result match {
            case TopValue | BotValue => result
            case Bool(b)             => Bool(BoolLattice[B].not(b))
          }

        case (Prim("odd?"), List(Opqs(_) | TopValue)) => TopValue
        case (Prim("odd?"), _)                        => BotValue
      }

    /**
      * Everything is true except for bottom `false`
      */
    def isTrue(value: Value): Boolean = value match {
      case BotValue => false
      case Bool(b)  => BoolLattice[B].isTrue(b)
      case _        => true
    }

    /**
      * Only bottom and `false` are false
      */
    def isFalse(value: Value): Boolean = value match {
      case BotValue => true
      case Bool(b)  => BoolLattice[B].isFalse(b)
      case _        => false
    }

    def subsumes(x: Value, y: Value): Boolean = (x, y) match {
      case (_, _) if x == y       => true
      case (TopValue, _)          => true
      case (Number(a), Number(b)) => IntLattice[I].subsumes(a, b)
      case (Bool(a), Bool(b))     => BoolLattice[B].subsumes(a, b)
      case (Grds(a), Grds(b))     => b.subsetOf(a)
      case (Arrs(a), Arrs(b))     => b.subsetOf(a)
      case (Clos(a), Clos(b))     => b.subsetOf(a)
      case (Opqs(a), Opqs(b))     => b.subsetOf(a)
      case (Prims(a), Prims(b))   => b.subsetOf(a)
      case (_, _)                 => false
    }
  }
}
class ScCoProductLattice[I, B, Addr <: Address](
    implicit val intLattice: IntLattice[I],
    implicit val boolLattice: BoolLattice[B]
) extends ScDomain[I, B, Addr] {
  import ScLattice._

  sealed trait CoProductValue
  case class CoProduct(value: Value) extends CoProductValue
  case object Top                    extends CoProductValue
  case object Bottom                 extends CoProductValue

  implicit val isScLattice = new ScLattice[CoProductValue, Addr] {
    implicit def intoCoProduct(value: Value): CoProductValue = value match {
      case TopValue => Top
      case BotValue => Bottom
      case _        => CoProduct(value)
    }

    override def injectBoolean(b: Boolean): CoProductValue = bool(b)

    override def injectInteger(n: Int): CoProductValue = number(n)

    override def injectClo(c: Clo[Addr]): CoProductValue = clo(c)

    override def injectGrd(g: Grd[Addr]): CoProductValue = grd(g)

    override def injectArr(a: Arr[Addr]): CoProductValue = arr(a)

    override def applyPrimitive(prim: Prim)(arguments: CoProductValue*): CoProductValue =
      Values.applyPrimitive(prim)(arguments.map {
        case product: CoProduct => product.value
        case Top                => TopValue
        case Bottom             => BotValue
      }: _*)

    override def isTrue(value: CoProductValue): Boolean = ???

    override def isFalse(value: CoProductValue): Boolean = ???

    override def isPrim(value: CoProductValue): Boolean = ???

    override def isClo(value: CoProductValue): Boolean = ???

    override def getPrim(value: CoProductValue): Set[Prim] = ???

    override def getClo(value: CoProductValue): Set[Clo[Addr]] = ???

    /** A lattice has a bottom element */
    override def bottom: CoProductValue = ???

    /** A lattice has a top element (might be undefined) */
    override def top: CoProductValue = ???

    /** Elements of the lattice can be joined together */
    override def join(x: CoProductValue, y: => CoProductValue): CoProductValue = (x, y) match {
      case (Top, _) | (_, Top)          => Top
      case (Bottom, _)                  => y
      case (_, Bottom)                  => x
      case (CoProduct(a), CoProduct(b)) => Values.join(a, b)
    }

    /** Subsumption between two elements can be checked */
    override def subsumes(x: CoProductValue, y: => CoProductValue): Boolean = ???

    override def show(v: CoProductValue): String = ???

    /** Equality check, returning an abstract result */
    override def eql[Bo: BoolLattice](x: CoProductValue, y: CoProductValue): Bo = ???

    override def injectSymbolic(sym: Symbolic): CoProductValue = Symbolics(Set(sym.expr))

    override def getGrd(value: CoProductValue): Set[Grd[Addr]] = ???

    override def getArr(value: CoProductValue): Set[Arr[Addr]] = ???
  }
}

class ScProductLattice[I, B, Addr <: Address](
    implicit val intLattice: IntLattice[I],
    implicit val boolLattice: BoolLattice[B]
) extends ScDomain[I, B, Addr] {
  import ScLattice._

  case class ProductElements(elements: List[Value])

  implicit val isScLattice: ScLattice[ProductElements, Addr] =
    new ScLattice[ProductElements, Addr]() {
      implicit def intoProductElements(value: Value): ProductElements =
        ProductElements(List(value))

      override def injectBoolean(b: Boolean): ProductElements =
        bool(b)

      override def injectInteger(n: Int): ProductElements =
        number(n)

      override def injectClo(c: Clo[Addr]): ProductElements =
        clo(c)

      override def injectGrd(g: Grd[Addr]): ProductElements =
        grd(g)

      override def injectArr(a: Arr[Addr]): ProductElements =
        arr(a)

      override def injectSymbolic(sym: Symbolic): ProductElements = Symbolics(Set(sym.expr))

      override def applyPrimitive(prim: Prim)(arguments: ProductElements*): ProductElements = {
        // TODO: this is wrong, fix
        val values = for {
          argument <- arguments
          value    <- argument.asInstanceOf[ProductElements].elements
        } yield (value)

        Values.applyPrimitive(prim)(values: _*)
      }

      override def isTrue(value: ProductElements): Boolean =
        value.elements.exists(Values.isTrue)

      override def isFalse(value: ProductElements): Boolean =
        value.elements.exists(Values.isFalse)

      override def isPrim(value: ProductElements): Boolean = ???

      override def isClo(value: ProductElements): Boolean = ???

      override def getPrim(value: ProductElements): Set[Prim] = ???

      override def getClo(value: ProductElements): Set[Clo[Addr]] = ???

      /** A lattice has a bottom element */
      override def bottom: ProductElements = ProductElements(List())

      /** A lattice has a top element (might be undefined) */
      override def top: ProductElements = throw LatticeTopUndefined

      /** Elements of the lattice can be joined together */
      override def join(x: ProductElements, y: => ProductElements): ProductElements = (x, y) match {
        case (ProductElements(x), ProductElements(y)) => ProductElements(append(x, y))
      }

      /** Subsumption between two elements can be checked */
      override def subsumes(x: ProductElements, y: => ProductElements): Boolean = ???

      /** Equality check, returning an abstract result */
      override def eql[B: BoolLattice](x: ProductElements, y: ProductElements): B = ???

      override def show(v: ProductElements): String = ???

      private def insert(x: Value, ys: List[Value]): List[Value] = ys match {
        case Nil                       => List(x)
        case z :: zs if x.ord == z.ord => Values.join(x, z) :: zs
        case z :: _ if z.ord < x.ord   => x :: ys
        case z :: zs                   => z :: insert(x, zs)
      }

      private def append(x: List[Value], y: List[Value]): List[Value] =
        x.foldLeft(y)((a, b) => insert(b, a))

      override def getGrd(value: ProductElements): Set[Grd[Addr]] = ???

      override def getArr(value: ProductElements): Set[Arr[Addr]] = ???
    }
}
