package maf.language.scheme.primitives

import maf.core._
import maf.language.scheme._
import maf.language.scheme.lattices.{SchemeLattice, SchemeOp}
import maf.util.Monoid

/** Help code for manually implementing Scheme primitives. */
trait PrimitiveBuildingBlocks[V, A <: Address] extends Serializable {

  val lat: SchemeLattice[V, A]
  lazy val latMon: Monoid[V] = maf.util.MonoidInstances.latticeMonoid[V](lat)
  lazy val mfMon: Monoid[MayFail[V, Error]] = maf.util.MonoidInstances.mayFail[V](latMon)

  import lat._

  implicit def buildMF[X](x: X): MayFail[X, Error] = MayFail.success(x)
  implicit def buildMF[X](err: Error): MayFail[X, Error] = MayFail.failure(err)

  /* Simpler names for frequently used lattice operations. */
  def isInteger: V => MayFail[V, Error] = x => op(SchemeOp.IsInteger)(List(x))
  def isVector: V => MayFail[V, Error] = x => op(SchemeOp.IsVector)(List(x))
  def isLock: V => MayFail[V, Error] = x => op(SchemeOp.IsLock)(List(x))
  def vectorLength: V => MayFail[V, Error] = x => op(SchemeOp.VectorLength)(List(x))
  def inexactToExact: V => MayFail[V, Error] = x => op(SchemeOp.InexactToExact)(List(x))
  def eqq: (V, V) => MayFail[V, Error] = (x, y) => op(SchemeOp.Eq)(List(x, y))

  def ifThenElse(cond: MayFail[V, Error])(thenBranch: => MayFail[V, Error])(elseBranch: => MayFail[V, Error]): MayFail[V, Error] =
    cond >>= { condv =>
      val t = if (isTrue(condv)) thenBranch else MayFail.success[V, Error](latMon.zero)
      val f = if (isFalse(condv)) elseBranch else MayFail.success[V, Error](latMon.zero)
      mfMon.append(t, f)
    }

  /** Dereferences a pointer x (which may point to multiple addresses) and applies a function to its value, joining everything together */
  def dereferencePointer(x: V, store: Store[A, V])(f: V => MayFail[V, Error]): MayFail[V, Error] =
    getPointerAddresses(x).foldLeft(MayFail.success[V, Error](bottom))((acc: MayFail[V, Error], a: A) =>
      for {
        v <- store.lookupMF(a)
        res <- f(v)
        accv <- acc
      } yield join(accv, res)
    )
  def dereferencePointerGetAddressReturnStore(
      x: V,
      store: Store[A, V]
    )(
      f: (A, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]
    ): MayFail[(V, Store[A, V]), Error] =
    getPointerAddresses(x).foldLeft(MayFail.success[(V, Store[A, V]), Error]((bottom, store)))((acc: MayFail[(V, Store[A, V]), Error], a: A) =>
      acc >>= { case (accv, updatedStore) =>
        /* We use the old store because the new added information can only negatively influence precision (as it didn't hold at the point of the function call). */
        store.lookupMF(a) >>= (v =>
          /* But we pass the updated store around as it should reflect all updates. */
          f(a, v, updatedStore) >>= { case (res, newStore) =>
            MayFail.success((join(accv, res), newStore))
          }
        )
      }
    )
}

class SchemeLatticePrimitives[V, A <: Address](implicit override val schemeLattice: SchemeLattice[V, A])
    extends SchemePrimitives[V, A]
       with PrimitiveBuildingBlocks[V, A] {
  val lat: SchemeLattice[V, A] = schemeLattice

  // See comments in SchemeR5RSBenchmarks.scala for a list of all supported and unsupported primitives
  def allPrimitives: Map[String, SchemePrimitive[V, A]] = {
    import PrimitiveDefs._
    ofList(List(
      `modulo`,
      `*`,
      `+`,
      `-`,
      `/`,
      `acos`,
      `asin`,
      `atan`,
      `boolean?`,
      `call/cc`,
      `car`,
      `cdr`,
      `ceiling`,
      `char->integer`,
      `char->string`,
      `char-ci<?`,
      `char-ci=?`,
      `char-downcase`,
      `char-lower-case?`,
      `char-upcase`,
      `char-upper-case?`,
      `char<?`,
      `char=?`,
      `char?`,
      `cons`,
      `cos`,
      `eq?`,
      `exact->inexact`,
      `expt`,
      `floor`,
      `inexact->exact`,
      `integer->char`,
      `integer?`,
      `list`,
      `log`,
      `make-string`,
      `max`,
      `min`,
      `null?`,
      `number->string`,
      `number?`,
      `pair?`,
      `procedure?`,
      `quotient`,
      `real?`,
      `remainder`,
      `round`,
      `set-car!`,
      `set-cdr!`,
      `sin`,
      `sqrt`,
      `string->number`,
      `string->symbol`,
      `string-append`,
      `string-length`,
      `string-ref`,
      `string-set!`,
      `string<?`,
      `string?`,
      `substring`,
      `symbol->string`,
      `symbol?`,
      `tan`,
      `make-vector`,
      `vector`,
      `vector-length`,
      `vector-ref`,
      `vector-set!`,
      `vector?`,
      `<`,
      `=`,
      /* IO primitives */
      `input-port?`,
      `output-port?`,
      `open-input-file`,
      `open-input-string`,
      `open-output-file`,
      `close-input-port`,
      `close-output-port`,
      `current-input-port`,
      `current-output-port`,
      `read-char`,
      `peek-char`,
      `write-char`,
      `read`,
      `write`,
      `display`,
      `eof-object?`,
      /* Other primitives that are not R5RS */
      `random`,
      `error`
    ) ++ CSchemePrimitives)

  }

  /** Primitives for a concurrent Scheme that are not part of R5RS. */
  def CSchemePrimitives: List[SchemePrimitive[V, A]] = {
    import PrimitiveDefs._
    List(
      `new-lock`,
      `acquire`,
      `release`,
      `lock?`,
      `thread?`
    )
  }

  class NoStore0Operation(val name: String, val call: () => MayFail[V, Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case Nil => call().map(v => (v, store))
      case _   => MayFail.failure(PrimitiveArityError(name, 0, args.length))
    }
  }

  class NoStore1Operation(val name: String, val call: V => MayFail[V, Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2).map(v => (v, store))
      case _        => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  class NoStore2Operation(val name: String, val call: (V, V) => MayFail[V, Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: Nil => call(x._2, y._2).map(v => (v, store))
      case _             => MayFail.failure(PrimitiveArityError(name, 2, args.length))
    }
  }

  class NoStore3Operation(val name: String, val call: (V, V, V) => MayFail[V, Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: z :: Nil => call(x._2, y._2, z._2).map(v => (v, store))
      case _                  => MayFail.failure(PrimitiveArityError(name, 3, args.length))
    }
  }

  abstract class NoStoreLOperation(val name: String) extends SchemePrimitive[V, A] {
    def call(args: List[V]): MayFail[V, Error]
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] =
      call(args.map(_._2)).map(v => (v, store))
  }

  class NoStoreLOp(val n: String, val c: List[V] => MayFail[V, Error]) extends NoStoreLOperation(n) {
    def call(args: List[V]): MayFail[V, Error] = c(args)
  }

  class NoStoreLOpRec(val n: String, val c: (List[V], List[V] => MayFail[V, Error]) => MayFail[V, Error]) extends NoStoreLOperation(n) {
    def call(args: List[V]): MayFail[V, Error] = c(args, call)
  }

  class Store1Operation(val name: String, val call: (V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2, store)
      case _        => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  class Scheme1Operation(val name: String, val call: (V, Store[A, V], SchemeInterpreterBridge[V, A]) => MayFail[(V, Store[A, V]), Error])
      extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        scheme: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2, store, scheme)
      case _        => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  class Store2Operation(val name: String, val call: (V, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: Nil => call(x._2, y._2, store)
      case _             => MayFail.failure(PrimitiveArityError(name, 2, args.length))
    }
  }

  class Store3Operation(val name: String, val call: (V, V, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]) extends SchemePrimitive[V, A] {
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeInterpreterBridge[V, A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: z :: Nil => call(x._2, y._2, z._2, store)
      case _                  => MayFail.failure(PrimitiveArityError(name, 3, args.length))
    }
  }

  object PrimitiveDefs extends PrimitiveBuildingBlocks[V, A] {

    val lat: SchemeLattice[V, A] = schemeLattice

    import schemeLattice._
    val unspecified = bool(false) /* TODO: introduce an "unspecified" value */

    def unaryOp(op: SchemeOp.SchemeOp1)(x: V): MayFail[V, Error] = lat.op(op)(List(x))
    def binaryOp(op: SchemeOp.SchemeOp2)(x: V, y: V): MayFail[V, Error] = lat.op(op)(List(x, y))
    def ternaryOp(
        op: SchemeOp.SchemeOp3
      )(
        x: V,
        y: V,
        z: V
      ): MayFail[V, Error] = lat.op(op)(List(x, y, z))

    case object `<` extends NoStore2Operation("<", binaryOp(SchemeOp.Lt)) // TODO[easy]: < should accept any number of arguments (same for <= etc.)
    case object `acos` extends NoStore1Operation("acos", unaryOp(SchemeOp.ACos))
    case object `asin` extends NoStore1Operation("asin", unaryOp(SchemeOp.ASin))
    case object `atan` extends NoStore1Operation("atan", unaryOp(SchemeOp.ATan))
    case object `boolean?` extends NoStore1Operation("boolean?", unaryOp(SchemeOp.IsBoolean))
    case object `ceiling` extends NoStore1Operation("ceiling", unaryOp(SchemeOp.Ceiling))
    case object `char->integer` extends NoStore1Operation("char->integer", unaryOp(SchemeOp.CharacterToInteger))
    case object `char-ci<?` extends NoStore2Operation("char-ci<?", binaryOp(SchemeOp.CharacterLtCI))
    case object `char-ci=?` extends NoStore2Operation("char-ci=?", binaryOp(SchemeOp.CharacterEqCI))
    case object `char-downcase` extends NoStore1Operation("char-downcase", unaryOp(SchemeOp.CharacterDowncase))
    case object `char-lower-case?` extends NoStore1Operation("char-lower-case?", unaryOp(SchemeOp.CharacterIsLower))
    case object `char-upcase` extends NoStore1Operation("char-upcase", unaryOp(SchemeOp.CharacterUpcase))
    case object `char-upper-case?` extends NoStore1Operation("char-upper-case?", unaryOp(SchemeOp.CharacterIsUpper))
    case object `char<?` extends NoStore2Operation("char<?", binaryOp(SchemeOp.CharacterLt))
    case object `char=?` extends NoStore2Operation("char=?", binaryOp(SchemeOp.CharacterEq))
    case object `char?` extends NoStore1Operation("char?", unaryOp(SchemeOp.IsChar))
    case object `cos` extends NoStore1Operation("cos", unaryOp(SchemeOp.Cos))
    case object `eq?` extends NoStore2Operation("eq?", eqq)
    case object `error` extends NoStore1Operation("error", x => MayFail.failure(UserError(x.toString)))
    case object `exact->inexact` extends NoStore1Operation("exact->inexact", unaryOp(SchemeOp.ExactToInexact))
    case object `expt` extends NoStore2Operation("expt", binaryOp(SchemeOp.Expt))
    case object `floor` extends NoStore1Operation("floor", unaryOp(SchemeOp.Floor))
    case object `inexact->exact` extends NoStore1Operation("inexact->exact", unaryOp(SchemeOp.InexactToExact))
    case object `integer->char` extends NoStore1Operation("integer->char", unaryOp(SchemeOp.IntegerToCharacter))
    case object `integer?` extends NoStore1Operation("integer?", unaryOp(SchemeOp.IsInteger))
    case object `log` extends NoStore1Operation("log", unaryOp(SchemeOp.Log))
    case object `modulo` extends NoStore2Operation("modulo", binaryOp(SchemeOp.Modulo))
    case object `null?` extends NoStore1Operation("null?", unaryOp(SchemeOp.IsNull))
    case object `number?` extends NoStore1Operation("number?", unaryOp(SchemeOp.IsReal))
    case object `real?` extends NoStore1Operation("real?", unaryOp(SchemeOp.IsReal))
    /* No support for complex number, so number? is equivalent as real? */
    case object `procedure?` extends NoStore1Operation("procedure?", unaryOp(SchemeOp.IsProcedure))
    case object `quotient` extends NoStore2Operation("quotient", binaryOp(SchemeOp.Quotient))
    case object `random` extends NoStore1Operation("random", unaryOp(SchemeOp.Random))
    case object `remainder` extends NoStore2Operation("remainder", binaryOp(SchemeOp.Remainder))
    case object `round` extends NoStore1Operation("round", unaryOp(SchemeOp.Round))
    case object `sin` extends NoStore1Operation("sin", unaryOp(SchemeOp.Sin))
    case object `symbol?` extends NoStore1Operation("symbol?", unaryOp(SchemeOp.IsSymbol))
    case object `tan` extends NoStore1Operation("tan", unaryOp(SchemeOp.Tan))

    case object `+`
        extends NoStoreLOpRec("+",
                              {
                                case (Nil, _)          => number(0)
                                case (x :: rest, call) => call(rest) >>= (binaryOp(SchemeOp.Plus)(x, _))
                              }
        )

    case object `-`
        extends NoStoreLOp("-",
                           {
                             case Nil       => MayFail.failure(PrimitiveVariadicArityError("-", 1, 0))
                             case x :: Nil  => binaryOp(SchemeOp.Minus)(number(0), x)
                             case x :: rest => `+`.call(rest) >>= (binaryOp(SchemeOp.Minus)(x, _))
                           }
        )

    case object `*`
        extends NoStoreLOpRec("*",
                              {
                                case (Nil, _)          => number(1)
                                case (x :: rest, call) => call(rest) >>= (binaryOp(SchemeOp.Times)(x, _))
                              }
        )

    case object `/`
        extends NoStoreLOp("/",
                           {
                             case Nil => MayFail.failure(PrimitiveVariadicArityError("/", 1, 0))
                             case x :: rest =>
                               for {
                                 multrest <- `*`.call(rest)
                                 r <- binaryOp(SchemeOp.Div)(x, multrest)
                                 fl <- unaryOp(SchemeOp.Floor)(r)
                                 isexact <- eqq(r, fl)
                                 xisint <- isInteger(x)
                                 multrestisint <- isInteger(multrest)
                                 convert <- and(isexact, and(xisint, multrestisint))
                                 exr <- inexactToExact(r)
                                 res <- ifThenElse(convert)(exr)(r)
                               } yield res
                           }
        )

    case object `max`
        extends NoStoreLOpRec("max",
                              {
                                case (Nil, _)          => MayFail.failure(PrimitiveVariadicArityError("max", 1, 0))
                                case (x :: Nil, _)     => x
                                case (x :: rest, call) => call(rest) >>= { y => ifThenElse(`<`.call(x, y))(y)(x) }
                              }
        )

    case object `min`
        extends NoStoreLOpRec("min",
                              {
                                case (Nil, _)          => MayFail.failure(PrimitiveVariadicArityError("min", 1, 0))
                                case (x :: Nil, _)     => x
                                case (x :: rest, call) => call(rest) >>= { y => ifThenElse(`<`.call(x, y))(x)(y) }
                              }
        )

    case object `=` extends NoStoreLOperation("=") {
      def eq(first: V, l: List[V]): MayFail[V, Error] = l match {
        case Nil => bool(true)
        case x :: rest =>
          ifThenElse(binaryOp(SchemeOp.NumEq)(first, x)) {
            eq(first, rest)
          } {
            bool(false)
          }
      }
      override def call(args: List[V]): MayFail[V, Error] = args match {
        case Nil       => bool(true)
        case x :: rest => eq(x, rest)
      }
    }

    case object `sqrt`
        extends NoStore1Operation("sqrt",
                                  x =>
                                    ifThenElse(`<`.call(x, number(0))) {
                                      /* n < 0 */
                                      MayFail.failure(PrimitiveNotApplicable("sqrt", List(x)))
                                    } {
                                      /* n >= 0 */
                                      for {
                                        r <- unaryOp(SchemeOp.Sqrt)(x)
                                        fl <- unaryOp(SchemeOp.Floor)(r)
                                        argisexact <- isInteger(x)
                                        resisexact <- eqq(r, fl)
                                        convert <- and(argisexact, resisexact)
                                        exr <- inexactToExact(r)
                                        res <- ifThenElse(convert)(exr)(r)
                                      } yield res
                                    }
        )

    case object `pair?`
        extends Store1Operation("pair?",
                                (x, store) =>
                                  ifThenElse(unaryOp(SchemeOp.IsPointer)(x)) {
                                    dereferencePointer(x, store) { cons =>
                                      unaryOp(SchemeOp.IsCons)(cons)
                                    }
                                  } {
                                    bool(false)
                                  }.map(v => (v, store))
        )

    case object `vector?`
        extends Store1Operation("vector?",
                                (x, store) =>
                                  for {
                                    ispointer <- unaryOp(SchemeOp.IsPointer)(x)
                                    isvector <- dereferencePointer(x, store) { v =>
                                      isVector(v)
                                    }
                                  } yield (and(ispointer, isvector), store)
        )
    case object `thread?` extends NoStore1Operation("thread?", unaryOp(SchemeOp.IsThread))
    case object `lock?`
        extends Store1Operation("lock?",
                                (x, store) => // Analogous to `pair?`, could also do analogous to `vector?`.
                                  ifThenElse(unaryOp(SchemeOp.IsPointer)(x)) {
                                    dereferencePointer(x, store) { lock =>
                                      unaryOp(SchemeOp.IsLock)(lock)
                                    }
                                  } {
                                    bool(false)
                                  }.map(v => (v, store))
        )

    case object `string-append` extends SchemePrimitive[V, A] {
      val name = "string-append"
      private def buildString(args: List[V], store: Store[A, V]): MayFail[V, Error] = args match {
        case Nil => string("")
        case x :: otherArgs =>
          for {
            rest <- buildString(otherArgs, store)
            result <- dereferencePointer(x, store)(str => binaryOp(SchemeOp.StringAppend)(str, rest))
          } yield result
      }
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] =
        for {
          str <- buildString(args.map(_._2), store)
        } yield {
          val addr = alloc.pointer(fpos)
          (lat.pointer(addr), store.extend(addr, str))
        }
    }

    case object `make-string` extends SchemePrimitive[V, A] {
      val name = "make-string"
      private def makeString(
          length: V,
          char: V,
          fpos: SchemeExp,
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] =
        for {
          str <- binaryOp(SchemeOp.MakeString)(length, char)
        } yield {
          val addr = alloc.pointer(fpos)
          (lat.pointer(addr), store.extend(addr, str))
        }
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, length) :: Nil              => makeString(length, lat.char(0.toChar), fpos, store, alloc)
        case (_, length) :: (_, char) :: Nil => makeString(length, char, fpos, store, alloc)
        case l                               => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }
    }

    case object `number->string` extends SchemePrimitive[V, A] {
      val name = "number->string"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, number) :: Nil =>
          for {
            str <- unaryOp(SchemeOp.NumberToString)(number)
          } yield {
            val addr = alloc.pointer(fpos)
            (lat.pointer(addr), store.extend(addr, str))
          }
        case l => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }
    }

    case object `string->number`
        extends Store1Operation(
          "string->number",
          (x, store) => dereferencePointer(x, store)(unaryOp(SchemeOp.StringToNumber)).map(v => (v, store))
        )

    case object `string->symbol`
        extends Store1Operation(
          "string->symbol",
          (x, store) => dereferencePointer(x, store)(unaryOp(SchemeOp.StringToSymbol)).map(v => (v, store))
        )

    case object `string-length`
        extends Store1Operation(
          "string-length",
          (x, store) => dereferencePointer(x, store)(unaryOp(SchemeOp.StringLength)).map(v => (v, store))
        )

    case object `string-ref`
        extends Store2Operation(
          "string-ref",
          (x, y, store) => dereferencePointer(x, store)(binaryOp(SchemeOp.StringRef)(_, y)).map(v => (v, store))
        )

    case object `string-set!`
        extends Store3Operation(
          "string-set!",
          (x, idx, chr, store) =>
            dereferencePointerGetAddressReturnStore(x, store) { (addr, str, store) =>
              for {
                updatedStr <- ternaryOp(SchemeOp.StringSet)(str, idx, chr)
              } yield (unspecified, store.update(addr, updatedStr))
            }
        )

    case object `string<?`
        extends Store2Operation(
          "string<?",
          (x, y, store) =>
            dereferencePointer(x, store) { xstr =>
              dereferencePointer(y, store) { ystr =>
                binaryOp(SchemeOp.StringLt)(xstr, ystr)
              }
            }.map(v => (v, store))
        )

    case object `string?`
        extends Store1Operation(
          "string?",
          (x, store) =>
            ifThenElse(unaryOp(SchemeOp.IsPointer)(x)) {
              dereferencePointer(x, store)(unaryOp(SchemeOp.IsString))
            } {
              bool(false)
            }.map(v => (v, store))
        )

    case object `substring` extends SchemePrimitive[V, A] {
      val name = "substring"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, ptr) :: (_, start) :: (_, end) :: Nil =>
          for {
            substr <- dereferencePointer(ptr, store) { str =>
              ternaryOp(SchemeOp.Substring)(str, start, end)
            }
          } yield {
            val addr = alloc.pointer(fpos)
            (lat.pointer(addr), store.extend(addr, substr))
          }
        case _ => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }

    case object `symbol->string` extends SchemePrimitive[V, A] {
      val name = "symbol->string"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, sym) :: Nil =>
          for {
            str <- unaryOp(SchemeOp.SymbolToString)(sym)
          } yield {
            val addr = alloc.pointer(fpos)
            (lat.pointer(addr), store.extend(addr, str))
          }
        case _ => MayFail.failure(PrimitiveArityError(name, 1, args.size))
      }
    }

    case object `char->string` extends SchemePrimitive[V, A] {
      val name = "char->string"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, chr) :: Nil =>
          for {
            str <- unaryOp(SchemeOp.CharacterToString)(chr)
          } yield {
            val addr = alloc.pointer(fpos)
            (lat.pointer(addr), store.extend(addr, str))
          }
        case l => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }
    }

    case object `cons` extends SchemePrimitive[V, A] {
      val name = "cons"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val addr = alloc.pointer(fpos)
          val consVal = lat.cons(car, cdr)
          val pointer = lat.pointer(addr)
          (pointer, store.extend(addr, consVal))
        case l => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }

    case object `car` extends Store1Operation("car", (x, store) => dereferencePointer(x, store)(cons => lat.car(cons)).map(v => (v, store)))
    case object `cdr` extends Store1Operation("cdr", (x, store) => dereferencePointer(x, store)(cons => lat.cdr(cons)).map(v => (v, store)))

    case object `set-car!`
        extends Store2Operation("set-car!",
                                (cell, value, store) =>
                                  dereferencePointerGetAddressReturnStore(cell, store) { (addr, cons, store) =>
                                    for {
                                      cdr <- lat.cdr(cons)
                                      updated = lat.cons(value, cdr)
                                    } yield (unspecified, store.update(addr, updated))
                                  }
        )

    case object `set-cdr!`
        extends Store2Operation("set-cdr!",
                                (cell, value, store) =>
                                  dereferencePointerGetAddressReturnStore(cell, store) { (addr, cons, store) =>
                                    for {
                                      car <- lat.car(cons)
                                      updated = lat.cons(car, value)
                                    } yield (unspecified, store.update(addr, updated))
                                  }
        )

    case object `make-vector` extends SchemePrimitive[V, A] {
      val name = "make-vector"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = {
        def createVec(size: V, init: V): MayFail[(V, Store[A, V]), Error] =
          isInteger(size) >>= (isint =>
            if (isTrue(isint)) {
              val veca = alloc.pointer(fpos)
              lat.vector(size, init) >>= (vec => (pointer(veca), store.extend(veca, vec)))
            } else {
              MayFail.failure(PrimitiveNotApplicable(name, args.map(_._2)))
            }
          )
        args match {
          case (_, size) :: Nil              => createVec(size, unspecified)
          case (_, size) :: (_, init) :: Nil => createVec(size, init)
          case l                             => MayFail.failure(PrimitiveVariadicArityError(name, 1, l.size))
        }
      }
    }

    case object `vector` extends SchemePrimitive[V, A] {
      val name = "vector"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = {
        val veca = alloc.pointer(fpos)
        lat.vector(number(args.size), bottom) >>= (emptyvec =>
          args.zipWithIndex.foldLeft(MayFail.success[V, Error](emptyvec))((acc, arg) =>
            acc >>= (vec =>
              arg match {
                case ((_, value), index) =>
                  vectorSet(vec, number(index), value)
              }
            )
          )
        ) >>= (vec => (pointer(veca), store.extend(veca, vec)))
      }
    }

    case object `vector-length`
        extends Store1Operation("vector-length",
                                (v, store) =>
                                  dereferencePointer(v, store) { vec =>
                                    vectorLength(vec)
                                  }.map((_, store))
        )

    case object `vector-ref`
        extends Store2Operation("vector-ref",
                                (v, index, store) =>
                                  dereferencePointer(v, store) { vec =>
                                    lat.vectorRef(vec, index)
                                  }.map((_, store))
        )

    case object `vector-set!` extends SchemePrimitive[V, A] {
      def name = "vector-set!"
      def vectorSet(
          v: V,
          index: V,
          newval: V,
          store: Store[A, V]
        ): MayFail[(V, Store[A, V]), Error] =
        dereferencePointerGetAddressReturnStore(v, store) { case (veca, vec, store) =>
          isVector(vec) >>= (test => {
            val t: MayFail[(V, Option[(A, V)]), Error] =
              if (isTrue(test)) {
                lat.vectorSet(vec, index, newval) >>= (newvec => MayFail.success((unspecified, Some((veca, newvec)))))
              } else {
                MayFail.success((bottom, None))
              }
            val f: MayFail[V, Error] =
              if (isFalse(test)) {
                MayFail.failure(PrimitiveNotApplicable(name, List(v, index, newval)))
              } else {
                MayFail.success(bottom)
              }
            t >>= {
              case (tv, None) => f.join(MayFail.success(tv), join).map(v => (v, store))
              case (tv, Some((a, va))) =>
                f.join(MayFail.success(tv), join).map(v => (v, store.update(a, va)))
            }
          })
        }
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case v :: index :: newval :: Nil => vectorSet(v._2, index._2, newval._2, store)
        case _                           => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }

    case object `list` extends SchemePrimitive[V, A] {
      def name = "list"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil => (nil, store)
        case (argpos, v) :: rest =>
          for {
            (restv, store2) <- call(fpos, rest, store, alloc)
          } yield {
            val addr = alloc.pointer(argpos)
            val pair = lat.cons(v, restv)
            val updatedStore = store2.extend(addr, pair)
            val pointer = lat.pointer(addr)
            (pointer, updatedStore)
          }
      }
    }

    case object `call/cc` extends SchemePrimitive[V, A] {
      val name = "call/cc"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          scheme: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case (_, fun) :: Nil =>
          val closures = lat.getClosures(fun)
          val results = closures.collect { case (clo @ (SchemeLambda(_ :: Nil, _, _), _), nam) =>
            scheme.callcc(clo, nam, fpos.idn.pos)
          }
          (lat.join(results), store)
        case l => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }

    }

    case object `input-port?` extends NoStore1Operation("input-port?", x => unaryOp(SchemeOp.IsInputPort)(x))

    case object `output-port?` extends NoStore1Operation("output-port?", x => unaryOp(SchemeOp.IsOutputPort)(x))

    case object `open-input-file`
        extends Store1Operation("open-input-file",
                                (x, store) =>
                                  dereferencePointer(x, store) { str =>
                                    ifThenElse(unaryOp(SchemeOp.IsString)(str)) {
                                      for {
                                        // TODO: this could be cleaner by having a difference between a file input port and string input port, but this would be a bit overkill
                                        portstring <- binaryOp(SchemeOp.StringAppend)(string("__file__"), str)
                                        inputPort <- unaryOp(SchemeOp.MakeInputPort)(portstring)
                                      } yield inputPort
                                    } {
                                      MayFail.failure(PrimitiveNotApplicable("open-input-file", List(x)))
                                    }
                                  }.map(v => (v, store))
        )

    case object `open-input-string`
        extends Store1Operation("open-input-string",
                                (x, store) =>
                                  dereferencePointer(x, store) { str =>
                                    ifThenElse(unaryOp(SchemeOp.IsString)(str)) {
                                      unaryOp(SchemeOp.MakeInputPort)(str)
                                    } {
                                      MayFail.failure(PrimitiveNotApplicable("open-input-string", List(x)))
                                    }
                                  }.map(v => (v, store))
        )

    case object `open-output-file`
        extends Store1Operation("open-output-file",
                                (x, store) =>
                                  dereferencePointer(x, store) { str =>
                                    ifThenElse(unaryOp(SchemeOp.IsString)(str)) {
                                      unaryOp(SchemeOp.MakeOutputPort)(str)
                                    } {
                                      MayFail.failure(PrimitiveNotApplicable("open-output-file", List(x)))
                                    }
                                  }.map(v => (v, store))
        )

    case object `close-input-port`
        extends NoStore1Operation("close-input-port",
                                  x =>
                                    ifThenElse(unaryOp(SchemeOp.IsInputPort)(x)) {
                                      unspecified
                                    } {
                                      MayFail.failure(PrimitiveNotApplicable("close-input-port", List(x)))
                                    }
        )

    case object `close-output-port`
        extends NoStore1Operation("close-output-port",
                                  x =>
                                    ifThenElse(unaryOp(SchemeOp.IsOutputPort)(x)) {
                                      unspecified
                                    } {
                                      MayFail.failure(PrimitiveNotApplicable("close-output-port", List(x)))
                                    }
        )

    case object `current-input-port` extends NoStore0Operation("current-input-port", () => unaryOp(SchemeOp.MakeInputPort)(string("__console__")))

    case object `current-output-port` extends NoStore0Operation("current-output-port", () => unaryOp(SchemeOp.MakeOutputPort)(string("__console__")))

    class ReadOrPeekChar(name: String) extends NoStoreLOperation(name) {
      def call(args: List[V]): MayFail[V, Error] = args match {
        case Nil => charTop
        case inputPort :: Nil =>
          ifThenElse(unaryOp(SchemeOp.IsInputPort)(inputPort)) {
            charTop
          } {
            MayFail.failure(PrimitiveNotApplicable(name, args))
          }
        case l => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }
    }

    case object `read-char` extends ReadOrPeekChar("read-char")
    case object `peek-char` extends ReadOrPeekChar("peek-char")

    case object `write-char` extends NoStoreLOperation("write-char") {
      def call(args: List[V]): MayFail[V, Error] = args match {
        case Nil => unspecified
        case outputPort :: Nil =>
          ifThenElse(unaryOp(SchemeOp.IsOutputPort)(outputPort)) {
            unspecified
          } {
            MayFail.failure(PrimitiveNotApplicable("write-char", args))
          }
        case l => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }
    }

    class WriteOrDisplay(name: String) extends NoStoreLOperation(name) {
      def call(args: List[V]): MayFail[V, Error] = args match {
        case _ :: Nil => unspecified
        case _ :: outputPort :: Nil =>
          ifThenElse(unaryOp(SchemeOp.IsOutputPort)(outputPort)) {
            unspecified
          } {
            MayFail.failure(PrimitiveNotApplicable(name, args))
          }
        case l => MayFail.failure(PrimitiveArityError(name, 1, l.size))
      }
    }

    case object `display` extends WriteOrDisplay("display")
    case object `write` extends WriteOrDisplay("write")

    case object `read` extends SchemePrimitive[V, A] {
      val name = "read"
      def result(
          fpos: SchemeExp,
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): (V, Store[A, V]) = {
        val adr = alloc.pointer(fpos)
        val ptr = lat.pointer(adr)
        val vlu = lat.join(Seq(ptr, nil, numTop, realTop, charTop, symbolTop, boolTop))
        val cns = lat.cons(vlu, vlu)
        // Note #1: creating a vector with these arguments is known to succeed
        // Note #2: vector should use a different address than the cons-cell
        val MayFailSuccess(vct) = lat.vector(numTop, vlu)
        (vlu,
         store
           .extend(adr, cns)
           .extend(adr, vct)
           .extend(adr, stringTop)
        )
      }
      def argError(args: List[(SchemeExp, V)]) =
        PrimitiveNotApplicable("read", args)
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil =>
          result(fpos, store, alloc)
        case (_, inp) :: Nil =>
          unaryOp(SchemeOp.IsInputPort)(inp) >>= { bln =>
            (isTrue(bln), isFalse(bln)) match {
              case (true, true)   => result(fpos, store, alloc).addError(argError(args))
              case (true, false)  => result(fpos, store, alloc)
              case (false, true)  => argError(args)
              case (false, false) => (bottom, store)
            }
          }
        case oth => MayFail.failure(PrimitiveArityError(name, 1, oth.size))
      }
    }

    case object `eof-object?`
        extends NoStore1Operation("eof-object?",
                                  /* TODO: there is no specific encoding for EOF objects, but they can only arise in scenarios where charTop is produced. So we can approximate them as follows */
                                  x =>
                                    if (subsumes(x, charTop)) { boolTop }
                                    else { bool(false) }
        )

    case object `new-lock` extends SchemePrimitive[V, A] {
      val name = "new-lock"
      override def call(
          fpos: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeInterpreterBridge[V, A]
        ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil =>
          val addr = alloc.pointer(fpos)
          val lock = lat.lock(Set.empty) // An initial lock does not contain any thread since it is not held.
          val ptr = lat.pointer(addr)
          (ptr, store.extend(addr, lock))
        case l => MayFail.failure(PrimitiveArityError(name, 0, l.size))
      }
    }

    case object `acquire`
        extends Scheme1Operation("acquire",
                                 (lockPtr, store, scheme) =>
                                   dereferencePointerGetAddressReturnStore(lockPtr, store) { (addr, lock, store) =>
                                     for {
                                       locked <- lat.acquire(lock, scheme.currentThread)
                                     } yield (lat.void, store.update(addr, locked))
                                   }
        )

    case object `release`
        extends Scheme1Operation("release",
                                 (lockPtr, store, scheme) =>
                                   dereferencePointerGetAddressReturnStore(lockPtr, store) { (addr, lock, store) =>
                                     for {
                                       unlocked <- lat.release(lock, scheme.currentThread)
                                     } yield (lat.void, store.update(addr, unlocked))
                                   }
        )
  }
}
