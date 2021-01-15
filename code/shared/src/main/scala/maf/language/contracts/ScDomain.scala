package maf.language.contracts

import maf.core.{Address, LatticeTopUndefined}
import maf.lattice.interfaces.{BoolLattice, IntLattice}
import maf.util.SmartHash

trait ScDomain[I, B, Addr <: Address] {
  import ScLattice._

  implicit protected[this] def intLattice: IntLattice[I]
  implicit protected[this] def boolLattice: BoolLattice[B]

  sealed trait Value extends SmartHash {
    def ord: Int
  }

  val TOP_VALUE = 0
  val BOT_VALUE = 1
  val BOOL_VALUE = 2
  val CLOS_VALUE = 3
  val GRDS_VALUE = 4
  val ARRS_VALUE = 5
  val NUM_VALUE = 6
  val OPQS_VALUE = 7
  val PRIMS_VALUE = 8
  val BLAMES_VALUE = 9
  val SYM_VALUE = 10
  val FLAT_VALUE = 11
  val REFINED_VALUE_IN_STATE = 12
  val THUNK_VALUE = 13
  val CONS_VALUE = 14
  val NIL_VALUE = 15
  val VEC_VALUE = 16
  val PTR_VALUE = 17
  val SYMBOL_VALUE = 18

  case object TopValue extends Value {
    def ord = TOP_VALUE
    override def toString: String = "top"
  }

  case object BotValue extends Value {
    def ord = BOT_VALUE
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

  case class Symbols(symbols: Set[Symbol]) extends Value {
    def ord = SYMBOL_VALUE
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

  case class Ptr(addrs: Set[Addr]) extends Value {
    def ord: Int = PTR_VALUE
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

  def ptr(c: Addr): Ptr = Ptr(Set(c))

  def symbol(s: Symbol): Symbols = Symbols(Set(s))

  object Values {
    def join(a: Value, b: Value): Value = (a, b) match {
      case (TopValue, _) | (_, TopValue) => TopValue
      case (BotValue, _)                 => a
      case (_, BotValue)                 => b
      case (Number(_), Opqs(opqs)) if opqs.forall(_.refinementSet.contains("int?")) =>
        Opqs(opqs)
      case (Opqs(opqs), Number(_)) if opqs.forall(_.refinementSet.contains("int?")) =>
        Opqs(opqs)
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
      case (Symbols(a), Symbols(b))                       => Symbols(a ++ b)
      case (Nils, Nils)                                   => Nils
      case (Vec(size1, elements1), Vec(size2, elements2)) => ??? // TODO
      case (Ptr(a1), Ptr(a2))                             => Ptr(a1 ++ a2)
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

    private def arith(op: (I, I) => I)(a: Value, b: Value): Value = (a, b) match {
      case (Number(a), Number(b)) => Number(op(a, b))
      case (Number(_), Opqs(b)) if isRefinedTo(b, "int?") =>
        opq(Opq(Set("int?")))

      case (Opqs(b), Number(_)) if isRefinedTo(b, "int?") =>
        opq(Opq(Set("int?")))

      case (Opqs(a), Opqs(b)) if isRefinedTo(a, "int?") && isRefinedTo(b, "int?") =>
        opq(Opq(Set("int?")))

      case (TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_)) => TopValue
      case (_, _)                                                           => BotValue
    }

    private def cmp(op: (I, I) => B)(a: Value, b: Value): Value = (a, b) match {
      case (Number(a), Number(b)) => Bool(op(a, b))
      case (TopValue | Number(_) | Opqs(_), TopValue | Number(_) | Opqs(_)) =>
        Bool(BoolLattice[B].top)
      case (_, _) => BotValue
    }

    private def bUnOp(op: (B => B))(a: Value): Value = a match {
      case (Bool(a))                      => Bool(op(a))
      case (TopValue | Bool(_) | Opqs(_)) => TopValue
      case _                              => BotValue
    }

    def pred(b: PartialFunction[Value, Unit], refinement: Option[String] = None)(v: Value): Value =
      v match {
        case v if b.isDefinedAt(v) => bool(true)
        case Opqs(s) if refinement.isDefined && s.forall(_.refinementSet.contains(refinement.get)) =>
          bool(true)
        case TopValue | Opqs(_) => Bool(BoolLattice[B].top)
        case BotValue           => BotValue
        case _                  => bool(false)
      }

    def pred(b: PartialFunction[Value, Unit], refinement: String): (Value => Value) =
      pred(b, Some(refinement))

    def constantBin(v: Value)(a: Value, b: Value): Value = v

    def primOr(a: Value, b: Value): Value = (isTrue(a), isTrue(b), isFalse(a), isFalse(b)) match {
      case (true, _, false, _) => a
      /** (or 4 #f) = 4 * */
      case (_, true, _, false) => b
      /** (or #f 4) = 4 */
      case (false, false, true, true) => Bool(BoolLattice[B].inject(false))
      case _                          => Bool(BoolLattice[B].top)
    }

    def primAnd(a: Value, b: Value): Value = (isTrue(a), isTrue(b), isFalse(a), isFalse(b)) match {
      case (true, true, false, false)                    => b
      case (true, true, true, _) | (true, true, _, true) => Bool(BoolLattice[B].top)
      case _                                             => Bool(BoolLattice[B].inject(false))
    }

    val binPrimitives: Map[String, (Value, Value) => Value] = Map(
      "+" -> arith(IntLattice[I].plus),
      "-" -> arith(IntLattice[I].minus),
      "*" -> arith(IntLattice[I].times),
      "/" -> arith(IntLattice[I].quotient),
      "<" -> cmp(IntLattice[I].lt[B]),
      ">" -> constantBin(TopValue),
      "=<" -> ((a, b) => ???),
      ">=" -> ((a, b) => ???),
      "=" -> cmp(IntLattice[I].eql[B]),
      "or" -> primOr,
      "and" -> primAnd
    )

    def unaryPrim: Map[String, (Value) => Value] = Map(
      "symbol?" -> pred({ case Symbols(_) => }, "symbol?"),
      "even?" -> (a => ???),
      "odd?" -> (a => ???),
      "proc?" -> pred { case Clos(_) | Prims(_) | Arrs(_) | Flats(_) => },
      "dependent-contract?" -> pred({ case Grds(_) => }, "dependent-contract?"),
      "true?" -> pred({ case Bool(b) if BoolLattice[B].isTrue(b) => }, "true?"),
      "false?" -> pred({ case Bool(b) if BoolLattice[B].isFalse(b) => }, "false?"),
      "int?" -> pred({ case Number(_) => }, "int?"),
      "any?" -> pred { case a if a != BotValue => },
      "pair?" -> pred { case Conses(_) => },
      "bool?" -> pred { case Bool(_) => },
      "number?" -> pred({ case Number(_) => }, "int?"),
      "char?" -> (_ => Bool(BoolLattice[B].top)), // TODO
      "vector?" -> (_ => Bool(BoolLattice[B].top)), // TODO
      "string?" -> (_ => Bool(BoolLattice[B].top)), // TODO
      "not" -> bUnOp(BoolLattice[B].not),
      "string-length" -> (_ => Number(IntLattice[I].top)),
      "null?" -> pred { case Nils => },
      "nonzero?" -> ((value) =>
        value match {
          case Number(a) =>
            Bool(BoolLattice[B].not((IntLattice[I].eql(a, IntLattice[I].inject(0)))))
          case TopValue | Opqs(_) => Bool(BoolLattice[B].top)
        }
      )
    )

    def applyPrimitive(prim: Prim)(arguments: Value*): Value =
      if (arguments.size == 1) {
        unaryPrim(prim.operation).apply(arguments.head)
      } else {
        binPrimitives(prim.operation).apply(arguments.head, arguments.tail.head)
      }

    def isTrue(value: Value): Boolean = value match {
      case BotValue => false
      case Bool(b)  => BoolLattice[B].isTrue(b)
      case _        => true
    }

    def isFalse(value: Value): Boolean = value match {
      case TopValue | Opqs(_) => true
      case Bool(b)            => BoolLattice[B].isFalse(b)
      case _                  => false
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

    def isDefinitelyArrow(value: Value): Boolean = value match {
      case Arrs(_) => true
      case _       => false
    }

    def isSymbol(value: Value): Boolean = value match {
      case Symbols(_) => true
      case _          => false
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

    def isPtr(value: Value): Boolean = value match {
      case Ptr(_) => true
      case _      => false
    }

    def subsumes(x: Value, y: Value): Boolean = (x, y) match {
      case (_, _) if x == y         => true
      case (TopValue, _)            => true
      case (Number(a), Number(b))   => IntLattice[I].subsumes(a, b)
      case (Bool(a), Bool(b))       => BoolLattice[B].subsumes(a, b)
      case (Grds(a), Grds(b))       => b.subsetOf(a)
      case (Arrs(a), Arrs(b))       => b.subsetOf(a)
      case (Clos(a), Clos(b))       => b.subsetOf(a)
      case (Opqs(a), Opqs(b))       => b.subsetOf(a)
      case (Prims(a), Prims(b))     => b.subsetOf(a)
      case (Blames(a), Blames(b))   => b.subsetOf(a)
      case (Conses(a), Conses(b))   => b.subsetOf(a)
      case (Symbols(a), Symbols(b)) => b.subsetOf(a)
      case (_, _)                   => false
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
      case Ptr(addr)      => s"ptr[$addr]"
      case _              => x.toString
    }
  }
}
class ScCoProductLattice[I, B, Addr <: Address](
    implicit val intLattice: IntLattice[I],
    implicit val boolLattice: BoolLattice[B])
    extends ScDomain[I, B, Addr] {
  import ScLattice._

  sealed trait CoProductValue extends Serializable
  case class CoProduct(value: Value) extends CoProductValue
  case object Top extends CoProductValue
  case object Bottom extends CoProductValue

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

    def injectBoolean(b: Boolean): CoProductValue = bool(b)

    def injectInteger(n: Int): CoProductValue = number(n)

    def injectClo(c: Clo[Addr]): CoProductValue = clo(c)

    def injectGrd(g: Grd[Addr]): CoProductValue = grd(g)

    def injectArr(a: Arr[Addr]): CoProductValue = arr(a)

    def injectPrim(p: Prim): CoProductValue = prim(p)

    def injectSymbolic(sym: Symbolic): CoProductValue = Symbolics(Set(sym.expr))

    def injectBlame(b: Blame): CoProductValue = blame(b)

    def injectOpq(o: Opq): CoProductValue = opq(o)

    def injectFlat(f: Flat[Addr]): CoProductValue = flat(f)

    def injectRefinedValueInState(state: CoProductValue, value: Opq): CoProductValue =
      state match {
        case CoProduct(v) => CoProduct(RefinedValueInStates(Map(v -> Set(value))))
        case _            => Top
      }

    def injectThunk(t: Thunk[Addr]): CoProductValue = thunk(t)

    def injectCons(c: Cons[Addr]): CoProductValue = cons(c)

    def injectPointer(a: Addr): CoProductValue = ptr(a)

    def injectNil: CoProductValue = Nils

    def vector(length: CoProductValue, init: CoProductValue): CoProductValue = ???

    def injectSymbol(s: Symbol): CoProductValue = symbol(s)

    def vectorSet(
        vector: CoProductValue,
        index: CoProductValue,
        value: CoProductValue
      ): CoProductValue = ???

    def vectorRef(vector: CoProductValue, index: CoProductValue): CoProductValue = ???

    /*================================================================================================================*/

    def applyPrimitive(prim: Prim)(arguments: CoProductValue*): CoProductValue =
      Values.applyPrimitive(prim)(arguments.map {
        case product: CoProduct => product.value
        case Top                => TopValue
        case Bottom             => BotValue
      }: _*)
    /*================================================================================================================*/

    def isTrue(value: CoProductValue): Boolean = isPred(Values.isTrue, value)

    def isFalse(value: CoProductValue): Boolean = isPred(Values.isFalse, value)

    def isPrim(value: CoProductValue): Boolean = isPred(Values.isPrim, value)

    def isClo(value: CoProductValue): Boolean = isPred(Values.isClo, value)

    def isBlame(value: CoProductValue): Boolean = isPred(Values.isBlame, value)

    def isFlatContract(value: CoProductValue): Boolean = isPred(Values.isFlat, value)

    def isDefinitelyOpq(value: CoProductValue): Boolean = value match {
      case CoProduct(value) => Values.isDefinitelyOpq(value)
      case _                => false
    }

    def isDefinitelyArrow(value: CoProductValue): Boolean = value match {
      case CoProduct(value) => Values.isDefinitelyArrow(value)
      case _                => false
    }

    def isThunk(value: CoProductValue): Boolean = isPred(Values.isThunk, value)

    def isCons(value: CoProductValue): Boolean = isPred(Values.isCons, value)

    def isNil(value: CoProductValue): Boolean = isPred(Values.isNil, value)

    def isVec(value: CoProductValue): Boolean = isPred(Values.isVec, value)

    def isSymbol(value: CoProductValue): Boolean = isPred(Values.isSymbol, value)

    def isPointer(value: CoProductValue): Boolean = ???

    /*================================================================================================================*/

    def getPrim(value: CoProductValue): Set[Prim] = value match {
      case CoProduct(Prims(prims)) => prims
      case _                       => Set()
    }

    def getClo(value: CoProductValue): Set[Clo[Addr]] = value match {
      case CoProduct(Clos(clos)) => clos
      case _                     => Set()
    }

    def getGrd(value: CoProductValue): Set[Grd[Addr]] = value match {
      case CoProduct(Grds(grds)) => grds
      case _                     => Set()
    }

    def getArr(value: CoProductValue): Set[Arr[Addr]] = value match {
      case CoProduct(Arrs(arrs)) => arrs
      case _                     => Set()
    }

    def getBlames(value: CoProductValue): Set[Blame] = value match {
      case CoProduct(Blames(blames)) => blames
      case _                         => Set()
    }

    def getOpq(value: CoProductValue): Set[Opq] = value match {
      case CoProduct(Opqs(opqs)) => opqs
      case _                     => Set()
    }

    def getFlat(value: CoProductValue): Set[Flat[Addr]] = value match {
      case CoProduct(Flats(flats)) => flats
      case _                       => Set()
    }

    def getSymbolic(value: CoProductValue): Option[String] = value match {
      case CoProduct(value) => Values.getSymbolic(value)
      case _                => None
    }

    def getThunk(value: CoProductValue): Set[Thunk[Addr]] = value match {
      case CoProduct(Thunks(t)) => t
      case _                    => Set()
    }

    def getCons(value: CoProductValue): Set[Cons[Addr]] = value match {
      case CoProduct(Conses(c)) => c
      case _                    => Set()
    }

    def getPointers(value: CoProductValue): Set[Addr] = value match {
      case CoProduct(Ptr(ptr)) => ptr
      case _                   => Set()
    }

    def getRefinedValueInState(value: CoProductValue): Set[RefinedValueInState[CoProductValue]] =
      value match {
        case CoProduct(value) =>
          value match {
            case RefinedValueInStates(m) =>
              m.flatMap { case (key, value) =>
                value.map(RefinedValueInState(CoProduct(key), _))
              }.toSet
            case _ => Set()
          }
        case _ => Set()
      }

    def getSymbols(value: CoProductValue): Set[Symbol] = value match {
      case CoProduct(Symbols(s)) => s
      case _                     => Set()
    }

    /*================================================================================================================*/

    /** A lattice has a bottom element */
    def bottom: CoProductValue = Bottom

    /** A lattice has a top element (might be undefined) */
    def top: CoProductValue = Top
    def integerTop: CoProductValue = Number(IntLattice[I].top)
    def boolTop = Bool(BoolLattice[B].top)

    /** Elements of the lattice can be joined together */
    def join(x: CoProductValue, y: => CoProductValue): CoProductValue = (x, y) match {
      case (Top, _) | (_, Top)          => Top
      case (Bottom, _)                  => y
      case (_, Bottom)                  => x
      case (CoProduct(a), CoProduct(b)) => Values.join(a, b)
    }

    /** Subsumption between two elements can be checked */
    def subsumes(x: CoProductValue, y: => CoProductValue): Boolean = (x, y) match {
      case (Top, _)                     => true
      case (_, Bottom)                  => true
      case (_, Top)                     => false
      case (CoProduct(a), CoProduct(b)) => Values.subsumes(a, b)
      case (_, _)                       => false
    }

    def show(v: CoProductValue): String = v match {
      case Top              => TopValue.toString
      case Bottom           => BotValue.toString
      case CoProduct(value) => Values.show(value)
    }

    /** Equality check, returning an abstract result */
    def eql[Bo: BoolLattice](x: CoProductValue, y: CoProductValue): Bo = ???
  }
}

class ScProductLattice[I, B, Addr <: Address](
    implicit val intLattice: IntLattice[I],
    implicit val boolLattice: BoolLattice[B])
    extends ScDomain[I, B, Addr] {
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

      override def injectSymbol(s: Symbol): ProductElements = ???

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

      private def isPred(
          value: ProductElements,
          category: Int,
          pred: (Value => Boolean)
        ): Boolean =
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

      override def isSymbol(value: ProductElements): Boolean = ???

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

      /** Extract a set of blames from the abstract value */
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

      override def getSymbols(value: ProductElements): Set[Symbol] = ???

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
      override def integerTop = Number(IntLattice[I].top)
      override def boolTop = Bool(BoolLattice[B].top)

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

      /** Inject an opaque value from the given state in the abstract domain */
      override def injectRefinedValueInState(state: ProductElements, value: Opq): ProductElements =
        ???

      /** Extract the set of opaque values associated with the given state */
      override def getRefinedValueInState(
          state: ProductElements
        ): Set[RefinedValueInState[ProductElements]] = ???

      /** Inject a thunk in the abstract domain */
      override def injectThunk(thunk: Thunk[Addr]): ProductElements = ???

      /** Returns true if the value is possibly a thunk */
      override def isThunk(value: ProductElements): Boolean = ???

      /** Extracts the set of thunks from the abstract domain */
      override def getThunk(value: ProductElements): Set[Thunk[Addr]] = ???

      /** Inject a cons value in the abstract domain */
      override def injectCons(cons: Cons[Addr]): ProductElements = ???

      /** Returns true if the value is possibly a cons pair */
      override def isCons(value: ProductElements): Boolean = ???

      override def isDefinitelyArrow(value: ProductElements): Boolean = ???

      /** Extracts the set of cons pairs from the abstract value */
      override def getCons(value: ProductElements): Set[Cons[Addr]] = ???

      /** Inject an address in the abstract domain */
      override def injectPointer(a: Addr): ProductElements = ???

      /** Returns true if the value is possibly a vector */
      override def isVec(value: ProductElements): Boolean = ???

      /** Returns true if the the value is a wrapped pointer */
      override def isPointer(value: ProductElements): Boolean = ???

      /** Extract the pointers contained within the value from the abstract domain. */
      override def getPointers(value: ProductElements): Set[Addr] = ???

      /**
       * Create a vector from a length represented as an abstract value
       * and with the default abstract value of `L`
       */
      override def vector(length: ProductElements, init: ProductElements): ProductElements = ???

      /** Change the value of the vector `vector` on index `index` to value `value` */
      override def vectorSet(
          vector: ProductElements,
          index: ProductElements,
          value: ProductElements
        ): ProductElements = ???

      /** Retrieve a value on index `index` from  the vector */
      override def vectorRef(vector: ProductElements, index: ProductElements): ProductElements = ???
    }
}
