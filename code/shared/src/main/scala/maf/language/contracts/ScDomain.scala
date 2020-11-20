package maf.language.contracts

import maf.core.{Address, Identity, LatticeTopUndefined}
import maf.lattice.interfaces.{BoolLattice, IntLattice}
import maf.util.SmartHash

trait ScDomain[I, B, Addr <: Address] {
  import ScLattice._

  protected[this] implicit def intLattice: IntLattice[I]
  protected[this] implicit def boolLattice: BoolLattice[B]

  sealed trait Value extends SmartHash {
    def ord: Int
  }

  val TOP_VALUE              = 0
  val BOT_VALUE              = 1
  val BOOL_VALUE             = 2
  val CLOS_VALUE             = 3
  val GRDS_VALUE             = 4
  val ARRS_VALUE             = 5
  val NUM_VALUE              = 6
  val OPQS_VALUE             = 7
  val PRIMS_VALUE            = 8
  val BLAMES_VALUE           = 9
  val SYM_VALUE              = 10
  val FLAT_VALUE             = 11
  val REFINED_VALUE_IN_STATE = 12
  val THUNK_VALUE            = 13
  val CONS_VALUE             = 14
  val NIL_VALUE              = 15
  val VEC_VALUE              = 16

  case object TopValue extends Value {
    def ord                       = TOP_VALUE
    override def toString: String = "top"
  }

  case object BotValue extends Value {
    def ord               = BOT_VALUE
    override def toString = "bottom"
  }

  case class Bool(b: B) extends Value {
    def ord = BOOL_VALUE
    override def toString: String = (Values.isTrue(this), Values.isFalse(this)) match {
      case (true, false)  => "true"
      case (false, true)  => "false"
      case (true, true)   => s"Bool(${TopValue.toString})"
      case (false, false) => s"Bool(${BotValue.toString})"
    }
  }

  case class Clos(clos: Set[Clo[Addr]]) extends Value {
    def ord = CLOS_VALUE
  }

  case class Grds(grds: Set[Grd[Addr]]) extends Value {
    def ord = GRDS_VALUE
  }

  case class Arrs(arrs: Set[Arr[Addr]]) extends Value {
    def ord = ARRS_VALUE
  }

  case class Number(i: I) extends Value {
    def ord = NUM_VALUE
  }

  case class Opqs(opqs: Set[Opq]) extends Value {
    def ord = OPQS_VALUE
  }

  case class Prims(prims: Set[Prim]) extends Value {
    def ord = PRIMS_VALUE
  }

  case class Blames(blames: Set[Blame]) extends Value {
    def ord = BLAMES_VALUE
  }

  case class Symbolics(expr: Set[ScExp]) extends Value {
    def ord = SYM_VALUE
  }

  case class Flats(flats: Set[Flat[Addr]]) extends Value {
    def ord = FLAT_VALUE
  }

  case class RefinedValueInStates(value: Map[Value, Set[Opq]]) extends Value {
    def ord = REFINED_VALUE_IN_STATE
  }

  case class Thunks(thunk: Set[Thunk[Addr]]) extends Value {
    def ord: Int = THUNK_VALUE
  }

  case class Conses(cons: Set[Cons[Addr]]) extends Value {
    def ord: Int = CONS_VALUE
  }

  case object Nils extends Value {
    def ord: Int = NIL_VALUE
  }

  case class Vec(size: I, elements: Map[I, Value]) extends Value {
    def ord: Int = VEC_VALUE
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

  def blame(b: Blame) =
    Blames(Set(b))

  def vec(length: I, default: Value) =
    Vec(length, Map().withDefaultValue(default))

  def prim(p: Prim) = Prims(Set(p))

  def opq(o: Opq): Opqs = Opqs(Set(o))

  def flat(f: Flat[Addr]): Flats = Flats(Set(f))

  def thunk(t: Thunk[Addr]): Thunks = Thunks(Set(t))

  def cons(c: Cons[Addr]): Conses = Conses(Set(c))

  object Values {
    def join(a: Value, b: Value): Value = (a, b) match {
      case (TopValue, _) | (_, TopValue)                  => TopValue
      case (BotValue, _)                                  => a
      case (_, BotValue)                                  => b
      case (Number(a), Number(b))                         => Number(IntLattice[I].join(a, b))
      case (Bool(a), Bool(b))                             => Bool(BoolLattice[B].join(a, b))
      case (Clos(a), Clos(b))                             => Clos(a ++ b)
      case (Arrs(a), Arrs(b))                             => Arrs(a ++ b)
      case (Grds(a), Grds(b))                             => Grds(a ++ b)
      case (Prims(a), Prims(b))                           => Prims(a ++ b)
      case (Symbolics(a), Symbolics(b))                   => Symbolics(a ++ b)
      case (Blames(a), Blames(b))                         => Blames(a ++ b)
      case (Opqs(a), Opqs(b))                             => Opqs(a ++ b)
      case (Flats(a), Flats(b))                           => Flats(a ++ b)
      case (Thunks(a), Thunks(b))                         => Thunks(a ++ b)
      case (Conses(a), Conses(b))                         => Conses(a ++ b)
      case (Nils, Nils)                                   => Nils
      case (Vec(size1, elements1), Vec(size2, elements2)) => ???
      case (RefinedValueInStates(v1), RefinedValueInStates(v2)) =>
        RefinedValueInStates(
          (v1.keys ++ v2.keys)
            .map(key => key -> (v1.getOrElse(key, Set()) ++ v2.getOrElse(key, Set())))
            .toMap
        )

      case (_, _) => TopValue
    }

    def isRefinedTo(b: Set[Opq], refinement: String): Boolean =
      b.exists(_.refinementSet.contains(refinement))

    def applyPrimitive(prim: Prim)(arguments: Value*): Value =
      (prim, arguments.toList) match {
        case (Prim("+"), List(Number(a), Number(b))) => Number(IntLattice[I].plus(a, b))
        case (Prim("+"), List(TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_))) =>
          TopValue
        case (Prim("+"), _) => BotValue

        case (Prim("-"), List(Number(a), Number(b))) => Number(IntLattice[I].minus(a, b))
        case (Prim("-"), List(Opqs(a), Number(_))) if isRefinedTo(a, "int?") =>
          opq(Opq(Set("int?")))

        case (Prim("-"), List(Number(_), Opqs(b))) if isRefinedTo(b, "int?") =>
          opq(Opq(Set("int?")))

        case (Prim("-"), List(Opqs(a), Opqs(b)))
            if (isRefinedTo(a, "int?") && isRefinedTo(b, "int?")) =>
          opq(Opq(Set("int?")))

        case (Prim("-"), List(TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_))) =>
          TopValue
        case (Prim("-"), _) => BotValue

        case (Prim("*"), List(Number(a), Number(b))) => Number(IntLattice[I].times(a, b))
        case (Prim("*"), List(TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_))) =>
          TopValue
        case (Prim("*"), _) => BotValue

        case (Prim("/"), List(Number(a), Number(b))) => Number(IntLattice[I].quotient(a, b))
        case (Prim("/"), List(TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_))) =>
          Number(IntLattice[I].top)
        case (Prim("/"), _) => BotValue

        case (Prim("<"), List(Number(a), Number(b))) => Bool(IntLattice[I].lt(a, b))
        case (Prim("<"), List(Number(_) | TopValue | Opqs(_), Number(_) | TopValue | Opqs(_))) =>
          Bool(BoolLattice[B].top)
        case (Prim("<"), List(_, _)) => BotValue

        case (Prim(">"), List(_, _)) => Bool(BoolLattice[B].top)

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

        case (Prim("proc?"), List(Clos(_) | Prims(_) | Arrs(_) | Flats(_))) => bool(true)
        case (Prim("proc?"), List(TopValue | Opqs(_)))                      => Bool(BoolLattice[B].top)
        case (Prim("proc?"), List(_))                                       => bool(false)

        case (Prim("dependent-contract?"), List(Grds(_)))            => bool(true)
        case (Prim("dependent-contract?"), List(TopValue | Opqs(_))) => Bool(BoolLattice[B].top)
        case (Prim("dependent-contract?"), List(_))                  => bool(false)

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

        case (Prim("="), List(Number(a), Number(b))) => Bool(IntLattice[I].eql(a, b))
        case (Prim("="), List(TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_))) =>
          TopValue
        case (Prim("="), List(_, _)) => BotValue

        case (Prim("=<"), List(Number(a), Number(b))) =>
          val diff = IntLattice[I].minus(a, b)
          val b1   = IntLattice[I].eql(diff, IntLattice[I].inject(0))
          val b2   = IntLattice[I].lt(a, b)
          Bool(
            (
              BoolLattice[B].isTrue(b1),
              BoolLattice[B].isTrue(b2),
              BoolLattice[B].isFalse(b1),
              BoolLattice[B].isFalse(b2)
            ) match {
              case (true, _, false, false) => BoolLattice[B].inject(true)
              case (_, true, false, false) => BoolLattice[B].inject(true)
              case _                       => BoolLattice[B].top
            }
          )

        case (Prim("=<"), List(Number(_) | TopValue | Opqs(_), Number(_) | TopValue | Opqs(_))) =>
          println("here", arguments)
          Bool(BoolLattice[B].top)

        case (Prim("=<"), List(_, _)) => BotValue

        case (Prim("true?"), List(Bool(b)))            => bool(BoolLattice[B].isTrue(b))
        case (Prim("true?"), List(TopValue | Opqs(_))) => TopValue
        case (Prim("true?"), List(BotValue))           => bool(false)
        case (Prim("true?"), List(_))                  => bool(true)

        case (Prim("false?"), List(TopValue | Opqs(_))) => TopValue
        case (Prim("false?"), List(Bool(b)))            => bool(BoolLattice[B].isFalse(b))
        case (Prim("false?"), List(_))                  => bool(false)

        case (Prim("int?"), List(BotValue))  => Bool(BoolLattice[B].bottom)
        case (Prim("int?"), List(Number(_))) => bool(true)
        case (Prim("int?"), List(Opqs(opqs))) if opqs.forall(_.refinementSet.contains("int?")) =>
          bool(true)

        case (Prim("int?"), List(TopValue | Opqs(_))) => Bool(BoolLattice[B].top)
        case (Prim("int?"), _)                        => bool(false)

        case (Prim("any?"), List(BotValue)) => BotValue
        case (Prim("any?"), List(_))        => bool(true)

        case (Prim("nonzero?"), List(Number(a))) =>
          Bool(BoolLattice[B].not((IntLattice[I].eql(a, IntLattice[I].inject(0)))))
        case (Prim("nonzero?"), List(TopValue | Opqs(_))) => Bool(BoolLattice[B].top)

        case (Prim("and"), List(a, b)) =>
          (isTrue(a), isTrue(b), isFalse(a), isFalse(b)) match {
            case (true, true, false, false)                    => Bool(BoolLattice[B].inject(true))
            case (true, true, true, _) | (true, true, _, true) => Bool(BoolLattice[B].top)
            case _                                             => Bool(BoolLattice[B].inject(false))
          }

        case (Prim("not"), List(Bool(a)))            => Bool(BoolLattice[B].not(a))
        case (Prim("not"), List(TopValue | Opqs(_))) => Bool(BoolLattice[B].top)
        case (Prim("not"), _)                        => BotValue

        case (Prim("bool?"), List(Bool(_)))            => Bool(BoolLattice[B].inject(true))
        case (Prim("bool?"), List(TopValue | Opqs(_))) => Bool(BoolLattice[B].top)
        case (Prim("bool?"), _)                        => BotValue
      }

    def isTrue(value: Value): Boolean = value match {
      case BotValue => false
      case Bool(b)  => BoolLattice[B].isTrue(b)
      case _        => true
    }

    def isFalse(value: Value): Boolean = value match {
      case TopValue => true
      case Bool(b)  => BoolLattice[B].isFalse(b)
      case _        => false
    }

    def isPrim(value: Value): Boolean = value match {
      case TopValue | Prims(_) => true
      case _                   => false
    }

    def isClo(value: Value): Boolean = value match {
      case TopValue | _: Clos => true
      case _                  => false
    }

    def isBlame(value: Value): Boolean = value match {
      case TopValue | _: Blames => true
      case _                    => false
    }

    def isDefinitelyOpq(value: Value): Boolean = value match {
      case Opqs(_) => true
      case _       => false
    }

    def isFlat(value: Value): Boolean = value match {
      case TopValue | _: Flats => true
      case _                   => false
    }

    def isThunk(value: Value): Boolean = value match {
      case TopValue | Thunks(_) => true
      case _                    => false
    }

    def isCons(value: Value): Boolean = value match {
      case TopValue | Conses(_) => true
      case _                    => false
    }

    def isNil(value: Value): Boolean = value match {
      case Nils => true
      case _    => false
    }

    def isVec(value: Value): Boolean = value match {
      case Vec(_, _) => true
      case _         => false
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
      case (Blames(a), Blames(b)) => b.subsetOf(a)
      case (Conses(a), Conses(b)) => b.subsetOf(a)
      case (_, _)                 => false
    }

    def getSymbolic(x: Value): Option[String] = x match {
      case (Prims(prims)) if prims.size == 1 =>
        Some(prims.head.operation)
      case _ => None
    }

    def show(x: Value): String = x match {
      case TopValue       => x.toString
      case BotValue       => x.toString
      case Number(a)      => a.toString
      case Bool(true)     => "true"
      case Bool(false)    => "false"
      case Grds(grds)     => s"{${grds.map(_.toString).mkString(",")}"
      case Arrs(arrs)     => s"{${arrs.map(_.toString).mkString(",")}"
      case Clos(clos)     => s"{${clos.map(_.toString).mkString(",")}"
      case Opqs(opqs)     => s"{${opqs.map(_.toString).mkString(",")}"
      case Prims(prims)   => s"{${prims.map(_.toString).mkString(",")}"
      case Blames(blames) => s"{${blames.map(_.toString).mkString(",")}}"
      case _              => x.toString
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

  def isPred(pred: (Value => Boolean), value: CoProductValue): Boolean = value match {
    case Top              => true
    case Bottom           => false
    case CoProduct(value) => pred(value)
  }

  implicit val isScLattice = new ScLattice[CoProductValue, Addr] {
    implicit def intoCoProduct(value: Value): CoProductValue = {
      val result = value match {
        case TopValue => Top
        case BotValue => Bottom
        case _        => CoProduct(value)
      }
      result
    }

    /*================================================================================================================*/

    override def injectBoolean(b: Boolean): CoProductValue = bool(b)

    override def injectInteger(n: Int): CoProductValue = number(n)

    override def injectClo(c: Clo[Addr]): CoProductValue = clo(c)

    override def injectGrd(g: Grd[Addr]): CoProductValue = grd(g)

    override def injectArr(a: Arr[Addr]): CoProductValue = arr(a)

    override def injectPrim(p: Prim): CoProductValue = prim(p)

    override def injectSymbolic(sym: Symbolic): CoProductValue = Symbolics(Set(sym.expr))

    override def injectBlame(b: Blame): CoProductValue = blame(b)

    override def injectOpq(o: Opq): CoProductValue = opq(o)

    override def injectFlat(f: Flat[Addr]): CoProductValue = flat(f)

    override def injectRefinedValueInState(state: CoProductValue, value: Opq): CoProductValue =
      state match {
        case CoProduct(v) => CoProduct(RefinedValueInStates(Map(v -> Set(value))))
        case _            => Top
      }

    def injectThunk(t: Thunk[Addr]): CoProductValue = thunk(t)

    def injectCons(c: Cons[Addr]): CoProductValue = cons(c)

    def injectNil: CoProductValue = Nils

    /*================================================================================================================*/

    override def applyPrimitive(prim: Prim)(arguments: CoProductValue*): CoProductValue = {
      Values.applyPrimitive(prim)(arguments.map {
        case product: CoProduct => product.value
        case Top                => TopValue
        case Bottom             => BotValue
      }: _*)
    }
    /*================================================================================================================*/

    override def isTrue(value: CoProductValue): Boolean = isPred(Values.isTrue, value)

    override def isFalse(value: CoProductValue): Boolean = isPred(Values.isFalse, value)

    override def isPrim(value: CoProductValue): Boolean = isPred(Values.isPrim, value)

    override def isClo(value: CoProductValue): Boolean = isPred(Values.isClo, value)

    override def isBlame(value: CoProductValue): Boolean = isPred(Values.isBlame, value)

    override def isFlatContract(value: CoProductValue): Boolean = isPred(Values.isFlat, value)

    override def isDefinitelyOpq(value: CoProductValue): Boolean = value match {
      case CoProduct(value) => Values.isDefinitelyOpq(value)
      case _                => false
    }

    def isThunk(value: CoProductValue): Boolean = isPred(Values.isThunk, value)

    def isCons(value: CoProductValue): Boolean = isPred(Values.isCons, value)

    def isNil(value: CoProductValue): Boolean = isPred(Values.isNil, value)

    /*================================================================================================================*/

    override def getPrim(value: CoProductValue): Set[Prim] = value match {
      case CoProduct(Prims(prims)) => prims
      case _                       => Set()
    }

    override def getClo(value: CoProductValue): Set[Clo[Addr]] = value match {
      case CoProduct(Clos(clos)) => clos
      case _                     => Set()
    }

    override def getGrd(value: CoProductValue): Set[Grd[Addr]] = value match {
      case CoProduct(Grds(grds)) => grds
      case _                     => Set()
    }

    override def getArr(value: CoProductValue): Set[Arr[Addr]] = value match {
      case CoProduct(Arrs(arrs)) => arrs
      case _                     => Set()
    }

    override def getBlames(value: CoProductValue): Set[Blame] = value match {
      case CoProduct(Blames(blames)) => blames
      case _                         => Set()
    }

    override def getOpq(value: CoProductValue): Set[Opq] = value match {
      case CoProduct(Opqs(opqs)) => opqs
      case _                     => Set()
    }

    override def getFlat(value: CoProductValue): Set[Flat[Addr]] = value match {
      case CoProduct(Flats(flats)) => flats
      case _                       => Set()
    }

    override def getSymbolic(value: CoProductValue): Option[String] = value match {
      case CoProduct(value) => Values.getSymbolic(value)
      case _                => None
    }

    def getRefinedValueInState(value: CoProductValue): Set[RefinedValueInState[CoProductValue]] =
      value match {
        case CoProduct(value) =>
          value match {
            case RefinedValueInStates(m) =>
              m.flatMap {
                case (key, value) => value.map(RefinedValueInState(CoProduct(key), _))
              }.toSet
            case _ => Set()
          }
        case _ => Set()
      }

    def getThunk(value: CoProductValue): Set[Thunk[Addr]] = value match {
      case CoProduct(Thunks(t)) => t
      case _                    => Set()
    }

    def getCons(value: CoProductValue): Set[Cons[Addr]] = value match {
      case CoProduct(Conses(c)) => c
      case _                    => Set()
    }

    /*================================================================================================================*/

    /** A lattice has a bottom element */
    override def bottom: CoProductValue = Bottom

    /** A lattice has a top element (might be undefined) */
    override def top: CoProductValue        = Top
    override def integerTop: CoProductValue = Number(IntLattice[I].top)
    override def boolTop                    = Bool(BoolLattice[B].top)

    /** Elements of the lattice can be joined together */
    override def join(x: CoProductValue, y: => CoProductValue): CoProductValue = (x, y) match {
      case (Top, _) | (_, Top)          => Top
      case (Bottom, _)                  => y
      case (_, Bottom)                  => x
      case (CoProduct(a), CoProduct(b)) => Values.join(a, b)
    }

    /** Subsumption between two elements can be checked */
    override def subsumes(x: CoProductValue, y: => CoProductValue): Boolean = (x, y) match {
      case (Top, _)                     => true
      case (_, Bottom)                  => true
      case (_, Top)                     => false
      case (CoProduct(a), CoProduct(b)) => Values.subsumes(a, b)
      case (_, _)                       => false
    }

    override def show(v: CoProductValue): String = v match {
      case Top              => TopValue.toString
      case Bottom           => BotValue.toString
      case CoProduct(value) => Values.show(value)
    }

    /** Equality check, returning an abstract result */
    override def eql[Bo: BoolLattice](x: CoProductValue, y: CoProductValue): Bo = ???

    /**
      * Inject an address in the abstract domain
      */
    override def injectPointer(a: Addr): CoProductValue = ???

    /**
      * Returns true if the value is possibly a vector
      */
    override def isVec(value: CoProductValue): Boolean = ???

    /**
      * Returns true if the the value is a wrapped pointer
      */
    override def isPointer(value: CoProductValue): Boolean = ???

    /**
      * Extract the pointers contained within the value from the abstract domain.
      */
    override def getPointers(value: CoProductValue): Set[Addr] = ???

    /**
      * Create a vector from a length represented as an abstract value
      * and with the default abstract value of `L`
      */
    override def vector(length: CoProductValue, init: CoProductValue): CoProductValue = ???

    /**
      * Change the value of the vector `vector` on index `index` to value `value`
      */
    override def vectorSet(
        vector: CoProductValue,
        index: CoProductValue,
        value: CoProductValue
    ): CoProductValue = ???

    /**
      * Retrieve a value on index `index` from  the vector
      */
    override def vectorRef(vector: CoProductValue, index: CoProductValue): CoProductValue = ???
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

      override def injectPrim(p: Prim): ProductElements =
        prim(p)

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

      override def injectBlame(b: Blame): ProductElements =
        blame(b)

      override def injectOpq(op: Opq): ProductElements = opq(op)

      override def injectSymbolic(sym: Symbolic): ProductElements = Symbolics(Set(sym.expr))

      override def injectFlat(f: Flat[Addr]): ProductElements =
        flat(f)

      override def injectNil: ProductElements = ???

      /*==============================================================================================================*/

      private def collectArgumentsList(arguments: List[ProductElements]): List[List[Value]] = {
        val heads = arguments.map(_.elements.head)
        val tails = arguments.map(_.elements.tail).map(ProductElements)
        heads :: collectArgumentsList(tails)
      }

      override def applyPrimitive(prim: Prim)(arguments: ProductElements*): ProductElements = {
        val results =
          collectArgumentsList(arguments.toList)
            .map((args) => Values.applyPrimitive(prim)(args: _*))
            .map((value) => ProductElements(List(value)))

        results.foldLeft(bottom)((result, value) => join(result, value))
      }

      /*==============================================================================================================*/

      private def isPred(value: ProductElements, category: Int, pred: (Value => Boolean)): Boolean =
        value.elements.exists(v => v.ord == category && pred(v))

      override def isTrue(value: ProductElements): Boolean =
        isPred(value, BOOL_VALUE, Values.isTrue)

      override def isFalse(value: ProductElements): Boolean =
        isPred(value, BOOL_VALUE, Values.isFalse)

      override def isPrim(value: ProductElements): Boolean =
        isPred(value, PRIMS_VALUE, Values.isPrim)

      override def isClo(value: ProductElements): Boolean =
        isPred(value, CLOS_VALUE, Values.isClo)

      override def isBlame(value: ProductElements): Boolean =
        isPred(value, BLAMES_VALUE, Values.isBlame)

      override def isFlatContract(value: ProductElements): Boolean =
        isPred(value, FLAT_VALUE, Values.isFlat)

      override def isDefinitelyOpq(value: ProductElements): Boolean = value match {
        case ProductElements(elements) => elements.forall((e) => Values.isDefinitelyOpq(e))
        case _                         => false
      }

      override def isNil(value: ProductElements): Boolean = ???

      /*==============================================================================================================*/

      override def getPrim(value: ProductElements): Set[Prim] =
        value.elements
          .flatMap {
            case p: Prims => Some(p.prims)
            case _        => None
          }
          .flatten
          .toSet

      override def getClo(value: ProductElements): Set[Clo[Addr]] =
        value.elements
          .flatMap {
            case c: Clos => Some(c.clos)
            case _       => None
          }
          .flatten
          .toSet

      override def getGrd(value: ProductElements): Set[Grd[Addr]] =
        value.elements
          .flatMap {
            case g: Grds => Some(g.grds)
            case _       => None
          }
          .flatten
          .toSet

      override def getArr(value: ProductElements): Set[Arr[Addr]] =
        value.elements
          .flatMap {
            case c: Arrs => Some(c.arrs)
            case _       => None
          }
          .flatten
          .toSet

      /**
        * Extract a set of blames from the abstract value
        */
      override def getBlames(value: ProductElements): Set[Blame] =
        value.elements
          .flatMap {
            case c: Blames => Some(c.blames)
            case _         => None
          }
          .flatten
          .toSet

      override def getOpq(value: ProductElements): Set[Opq] =
        value.elements
          .flatMap {
            case c: Opqs => Some(c.opqs)
            case _       => None
          }
          .flatten
          .toSet

      override def getFlat(value: ProductElements): Set[Flat[Addr]] =
        value.elements
          .flatMap {
            case c: Flats => Some(c.flats)
            case _        => None
          }
          .flatten
          .toSet

      override def getSymbolic(value: ProductElements): Option[String] =
        value.elements match {
          case List(value) => Values.getSymbolic(value)
          case _           => None
        }

      /*==============================================================================================================*/

      /** A lattice has a bottom element */
      override def bottom: ProductElements = ProductElements(List())

      /** A lattice has a top element (might be undefined) */
      override def top: ProductElements = throw LatticeTopUndefined
      override def integerTop           = Number(IntLattice[I].top)
      override def boolTop              = Bool(BoolLattice[B].top)

      /** Elements of the lattice can be joined together */
      override def join(x: ProductElements, y: => ProductElements): ProductElements = (x, y) match {
        case (ProductElements(x), ProductElements(y)) => ProductElements(append(x, y))
      }

      /** Subsumption between two elements can be checked */
      override def subsumes(x: ProductElements, y: => ProductElements): Boolean =
        join(x, y) == x

      /** Equality check, returning an abstract result */
      override def eql[B: BoolLattice](x: ProductElements, y: ProductElements): B = ???

      override def show(v: ProductElements): String =
        v.elements.map(v => s"${Values.show(v)}").mkString(" x ")

      private def insert(x: Value, ys: List[Value]): List[Value] = ys match {
        case List()                    => List(x)
        case z :: zs if x.ord == z.ord => Values.join(x, z) :: zs
        case z :: _ if z.ord < x.ord   => x :: ys
        case z :: zs                   => z :: insert(x, zs)
      }

      private def append(x: List[Value], y: List[Value]): List[Value] =
        x.foldLeft(y)((a, b) => insert(b, a))

      /**
        * Inject an opaque value from the given state in the abstract domain
        */
      override def injectRefinedValueInState(state: ProductElements, value: Opq): ProductElements =
        ???

      /**
        * Extract the set of opaque values associated with the given state
        */
      override def getRefinedValueInState(
          state: ProductElements
      ): Set[RefinedValueInState[ProductElements]] = ???

      /**
        * Inject a thunk in the abstract domain
        */
      override def injectThunk(thunk: Thunk[Addr]): ProductElements = ???

      /**
        * Returns true if the value is possibly a thunk
        */
      override def isThunk(value: ProductElements): Boolean = ???

      /**
        * Extracts the set of thunks from the abstract domain
        */
      override def getThunk(value: ProductElements): Set[Thunk[Addr]] = ???

      /**
        * Inject a cons value in the abstract domain
        */
      override def injectCons(cons: Cons[Addr]): ProductElements = ???

      /**
        * Returns true if the value is possibly a cons pair
        */
      override def isCons(value: ProductElements): Boolean = ???

      /**
        * Extracts the set of cons pairs from the abstract value
        */
      override def getCons(value: ProductElements): Set[Cons[Addr]] = ???

      /**
        * Inject an address in the abstract domain
        */
      override def injectPointer(a: Addr): ProductElements = ???

      /**
        * Returns true if the value is possibly a vector
        */
      override def isVec(value: ProductElements): Boolean = ???

      /**
        * Returns true if the the value is a wrapped pointer
        */
      override def isPointer(value: ProductElements): Boolean = ???

      /**
        * Extract the pointers contained within the value from the abstract domain.
        */
      override def getPointers(value: ProductElements): Set[Addr] = ???

      /**
        * Create a vector from a length represented as an abstract value
        * and with the default abstract value of `L`
        */
      override def vector(length: ProductElements, init: ProductElements): ProductElements = ???

      /**
        * Change the value of the vector `vector` on index `index` to value `value`
        */
      override def vectorSet(
          vector: ProductElements,
          index: ProductElements,
          value: ProductElements
      ): ProductElements = ???

      /**
        * Retrieve a value on index `index` from  the vector
        */
      override def vectorRef(vector: ProductElements, index: ProductElements): ProductElements = ???
    }
}
