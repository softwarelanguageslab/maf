package scalaam.language.contracts

import scalaam.core.Address
import scalaam.lattice.{BoolLattice, IntLattice}

class ScCoProductLattice[I: IntLattice, B: BoolLattice, Addr <: Address] {
  sealed trait Value

  implicit val isScLattice: ScLattice[Value, Addr] = new ScLattice[Value, Addr] {

    case object Top extends Value {
      override def toString: String = "⊤"
    }
    case object Bot extends Value {
      override def toString = "⊥"
    }
    case class Bool(b: B) extends Value {
      override def toString: String = (isTrue(this), isFalse(this)) match {
        case (true, false)  => "true"
        case (false, true)  => "false"
        case (true, true)   => Top.toString
        case (false, false) => Bot.toString
      }
    }
    case class Clos(clos: Set[Clo])    extends Value
    case class Grds(grds: Set[Grd])    extends Value
    case class Arrs(arrs: Set[Arr])    extends Value
    case class Number(i: I)            extends Value
    case class Opqs(opqs: Set[Opq])    extends Value
    case class Prims(prims: Set[Prim]) extends Value

    override def injectBoolean(bool: Boolean): Value = Bool(BoolLattice[B].inject(bool))

    override def injectInteger(n: Int): Value =
      Number(IntLattice[I].inject(n))

    override def injectClo(clo: Clo): Value =
      Clos(Set(clo))

    override def injectGrd(grd: Grd): Value =
      Grds(Set(grd))

    override def injectArr(arr: Arr): Value =
      Arrs(Set(arr))

    override def applyPrimitive(prim: Prim)(arguments: Value*): Value =
      (prim, arguments.toList) match {
        case (Prim("+"), List(Number(a), Number(b)))             => Number(IntLattice[I].plus(a, b))
        case (Prim("+"), List(Top | Number(_), Top | Number(_))) => Top
        case (Prim("+"), _)                                      => Bot

        case (Prim("-"), List(Number(a), Number(b)))             => Number(IntLattice[I].minus(a, b))
        case (Prim("-"), List(Top | Number(_), Top | Number(_))) => Top
        case (Prim("-"), _)                                      => Bot

        case (Prim("*"), List(Number(a), Number(b)))             => Number(IntLattice[I].times(a, b))
        case (Prim("*"), List(Top | Number(_), Top | Number(_))) => Top
        case (Prim("*"), _)                                      => Bot

        case (Prim("/"), List(Number(a), Number(b)))             => Number(IntLattice[I].quotient(a, b))
        case (Prim("/"), List(Top | Number(_), Top | Number(_))) => Top
        case (Prim("/"), _)                                      => Bot

        case (Prim("even?"), List(Number(a))) =>
          val mod2         = IntLattice[I].modulo(a, IntLattice[I].inject(2))
          val possiblyEven = IntLattice[I].subsumes(mod2, IntLattice[I].inject(0))
          val possiblyOdd  = IntLattice[I].subsumes(mod2, IntLattice[I].inject(1))
          (possiblyEven, possiblyOdd) match {
            case (true, true)   => Top
            case (true, false)  => Bool(BoolLattice[B].inject(true))
            case (false, true)  => Bool(BoolLattice[B].inject(false))
            case (false, false) => Bot
          }

        case (Prim("even?"), List(Opqs(_) | Top)) => Top
        case (Prim("even?"), _)                   => Bot

        case (Prim("odd?"), List(Number(b))) =>
          val result = applyPrimitive(Prim("even?"))(Number(b))
          result match {
            case Top | Bot => result
            case Bool(b)   => Bool(BoolLattice[B].not(b))
          }

        case (Prim("odd?"), List(Opqs(_) | Top)) => Top
        case (Prim("odd?"), _)                   => Bot
      }

    /**
      * Everything is true except for bottom `false`
      */
    override def isTrue(value: Value): Boolean = value match {
      case Bot     => false
      case Bool(b) => BoolLattice[B].isTrue(b)
      case _       => true
    }

    /**
      * Only bottom and `false` are false
      */
    override def isFalse(value: Value): Boolean = value match {
      case Bot     => true
      case Bool(b) => BoolLattice[B].isFalse(b)
      case _       => false
    }

    /** A lattice has a bottom element */
    override def bottom: Value = Bot

    /** A lattice has a top element (might be undefined) */
    override def top: Value = Top

    /** Elements of the lattice can be joined together */
    override def join(x: Value, y: => Value): Value = (x, y) match {
      case (Top, _)               => Top
      case (_, Bot)               => x
      case (Bot, _)               => y
      case (Number(a), Number(b)) => Number(IntLattice[I].join(a, b))
      case (Bool(a), Bool(b))     => Bool(BoolLattice[B].join(a, b))
      case (Grds(a), Grds(b))     => Grds(a ++ b) // join is union for the powerset of grd
      case (Arrs(a), Arrs(b))     => Arrs(a ++ b)
      case (Clos(a), Clos(b))     => Clos(a ++ b)
      case (Opqs(a), Opqs(b))     => Opqs(a ++ b)
      case (Prims(a), Prims(b))   => Prims(a ++ b)
      // joining elements of different type always results in top
      case (_, _) => Top
    }

    /** Subsumption between two elements can be checked */
    override def subsumes(x: Value, y: => Value): Boolean = (x, y) match {
      case (_, _) if x == y       => true
      case (Top, _)               => true
      case (Number(a), Number(b)) => IntLattice[I].subsumes(a, b)
      case (Bool(a), Bool(b))     => BoolLattice[B].subsumes(a, b)
      case (Grds(a), Grds(b))     => b.subsetOf(a)
      case (Arrs(a), Arrs(b))     => b.subsetOf(a)
      case (Clos(a), Clos(b))     => b.subsetOf(a)
      case (Opqs(a), Opqs(b))     => b.subsetOf(a)
      case (Prims(a), Prims(b))   => b.subsetOf(a)
      case (_, _)                 => false
    }

    /** Equality check, returning an abstract result */
    override def eql[B: BoolLattice](x: Value, y: Value): B = ???

    override def show(v: Value): String = v.toString
  }
}
