package maf.language.scheme.interpreter

import maf.core.Identity
import maf.core.Position.Position
import maf.language.scheme._
import maf.lattice.{MathOps, NumOps}

trait ConcreteSchemePrimitives:
    this: BaseSchemeInterpreter[_] =>

    import NumOps._
    import ConcreteValues._

    object Primitives:
        //def primitiveMap: Map[String, Prim] = allPrimitives.map(p => (p.name, p)).toMap
        def allPrimitives: Map[String, Prim] = List(
          Times, /* [vv] *: Arithmetic */
          Plus, /* [vv] +: Arithmetic */
          Minus, /* [vv] -: Arithmetic */
          Div, /* [vx] /: Arithmetic (no support for fractions) */
          Abs, /* [vv] abs: Arithmetic */
          ACos, /* [vv] acos: Scientific */
          /* [x]  angle: Complex */
          //Append, /* [x]  append: Append/Reverse */
          /* [x]  apply: Fly Evaluation */
          ASin, /* [vv] asin: Scientific */
          //Assoc, /* [vv] assoc: Retrieving Alist Entries */
          //Assq, /* [vv] assq: Retrieving Alist Entries */
          /* [x]  assv: Retrieving Alist Entries */
          ATan, /* [vv] atan: Scientific */
          Booleanp, /* [vv] boolean?: Booleans */
          /* [x]  call-with-current-continuation: Continuations */
          /* [x]  call-with-input-file: File Ports */
          /* [x]  call-with-output-file: File Ports */
          /* [x]  call-with-values: Multiple Values */
          Car, /* [vv] car: Pairs */
          Cdr, /* [vv] cdr: Pairs */
          Ceiling, /* [vv] ceiling: Arithmetic */
          CharToInteger, /* [vv]  char->integer: Characters */
          CharToString,
          CharCILt, /* [x]  char-ci<?: Characters */
          CharCIEq, /* [x]  char-ci=?: Characters */
          /* [x]  char-downcase: Characters */
          /* [x]  char-lower-case?: Characters */
          /* [x]  char-ready?: Reading */
          /* [x]  char-upcase: Characters */
          /* [x]  char-upper-case?: Characters */
          CharLt, /* [x]  char<?: Characters */
          CharEq, /* [x]  char=?: Characters */
          /* [x]  char>=?: Characters */
          /* [x]  char>?: Characters */
          Charp, /* [vv] char?: Characters */
          /* [x]  close-input-port: Closing */
          /* [x]  close-output-port: Closing */
          /* [x]  complex?: Complex Numbers */
          Cons, /* [vv] cons: Pairs */
          Cos, /* [vv] cos: Scientific */
          /* [x]  current-input-port: Default Ports */
          /* [x]  current-output-port: Default Ports */
          /* [x]  dynamic-wind: Dynamic Wind */
          /* [x]  eof-object?: Reading */
          Eq, /* [vv] eq?: Equality */
          //Equal, /* [vx] equal?: Equality */
          /* [x]  eqv?: Equality */
          /* [x]  eval: Fly Evaluation */
          Evenp, /* [vv] even?: Integer Operations */
          ExactToInexact, /* [vv] exact->inexact: Exactness */
          /* [x]  exact?: Exactness */
          /* [x]  exp: Scientific */
          Expt, /* [vv] expt: Scientific */
          Floor, /* [vv] floor: Arithmetic */
          /* [x]  force: Delayed Evaluation */
          Gcd, /* [vx] gcd: Integer Operations */
          /* [x]  imag-part: Complex */
          InexactToExact, /* [vv] inexact->exact: Exactness */
          /* [x]  inexact?: Exactness */
          IntegerToChar, /* [vv]  integer->char: Characters */
          Integerp, /* [vv] integer?: Integers */
          /* [x]  interaction-environment: Fly Evaluation */
          /* [x]  lcm: Integer Operations */
          //Length, /* [vv] length: List Selection */
          ListPrim, /* [vv] list: List Constructors */
          /* [x]  list->string: String Constructors */
          /* [x]  list->vector: Vector Creation */
          //ListRef, /* [vv] list-ref: List Selection */
          /* [x]  list-tail: List Selection */
          //Listp, /* [vv] list?: List Predicates */
          /* [x]  load: Loading */
          Log, /* [vv] log: Scientific */
          /* [x]  magnitude: Complex */
          /* [x]  make-polar: Complex */
          /* [x]  make-rectangular: Complex */
          MakeString, /* [vv]  make-string: String Constructors */
          /* [x]  map: List Mapping */
          Max, /* [vv] max: Arithmetic */
          //Member, /* [vv] member: List Searching */
          //Memq, /* [v]  memq: List Searching */
          /* [x]  memv: List Searching */
          Min, /* [vv] min: Arithmetic */
          Modulo, /* [vv] modulo: Integer Operations */
          Negativep, /* [vv] negative?: Comparison */
          Not, /* [vv] not: Booleans */
          Nullp, /* [vv] null?: List Predicates */
          NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
          Numberp, /* [vv] number?: Numerical Tower */
          Oddp, /* [vv] odd?: Integer Operations */
          Pairp, /* [vv] pair?: Pairs */
          /* [x]  peek-char?: Reading */
          Positivep, /* [vv] positive?: Comparison */
          Procp, /* [x]  procedure?: Procedure Properties */
          Quotient, /* [vv] quotient: Integer Operations */
          /* [x]  rational?: Reals and Rationals */
          /* [x]  read: Scheme Read */
          /* [x]  read-char?: Reading */
          /* [x]  real-part: Complex */
          Realp, /* [vv] real?: Reals and Rationals */
          Remainder, /* [vv] remainder: Integer Operations */
          /* [x]  reverse: Append/Reverse */
          Round, /* [vv] round: Arithmetic */
          SetCar, /* [vv] set-car!: Pairs */
          SetCdr, /* [vv] set-cdr!: Pairs */
          Sin, /* [vv] sin: Scientific */
          Sqrt, /* [vv] sqrt: Scientific */
          /* [x]  string: String Constructors */
          /* [x]  string->list: List/String Conversion */
          StringToNumber, /* [x]  string->number: Conversion */
          StringToSymbol, /* [vv] string->symbol: Symbol Primitives */
          StringAppend, /* [vx] string-append: Appending Strings: only two arguments supported */
          /* [x]  string-ci<: String Comparison */
          /* [x]  string-ci>=?: String Comparison */
          /* [x]  string-ci>?: String Comparison */
          /* [x]  string-copy: String Selection */
          /* [x]  string-fill!: String Modification */
          StringLength, /* [vv] string-length: String Selection */
          StringRef, /* [x]  string-ref: String Selection */
          StringSet, /* [x]  string-set!: String Modification */
          StringLt, /* [vv]  string<?: String Comparison */
          Stringp, /* [vv]  string?: String Predicates */
          Substring, /* [x]  substring: String Selection */
          SymbolToString, /* [vv] symbol->string: Symbol Primitives */
          Symbolp, /* [vv] symbol?: Symbol Primitives */
          Tan, /* [vv] tan: Scientific */
          /* [x]  truncate: Arithmetic */
          /* [x]  values: Multiple Values */
          MakeVector, /* [vv] make-vector: Vector Creation */
          Vector, /* [vv] vector: Vector Creation */
          /* [x]  vector-fill!: Vector Accessors */
          VectorLength, /* [vv] vector-length: Vector Accessors */
          VectorRef, /* [vv] vector-ref: Vector Accessors */
          VectorSet, /* [vv] vector-set!: Vector Accessors */
          Vectorp, /* [vv] vector?: Vector Creation */
          Zerop, /* [vv] zero?: Comparison */
          LessThan, /* [vv]  < */
          LessOrEqual, /* [vv]  <= */
          NumEq, /* [vv]  = */
          GreaterThan, /* [vv]  > */
          GreaterOrEqual, /* [vv]  >= */
          /* [x]  numerator */
          /* [x]  denominator */
          /* [x]  rationalize-string */
          /* [x]  scheme-report-environment */
          /* [x]  null-environment */
          /* [x]  write transcript-on */
          /* [x]  transcript-off */
          /* IO Primitives */
          `eof-object?`,
          `input-port?`,
          `output-port?`,
          `open-input-file`,
          `open-output-file`,
          `open-input-string`,
          `close-input-port`,
          `close-output-port`,
          `current-input-port`,
          `current-output-port`,
          `read-char`,
          `peek-char`,
          `read`,
          `write`,
          `write-char`,
          `display`,
          `newline`,
          /* [x]  with-input-from-file: File Ports */
          /* [x]  with-output-to-file: File Ports */
          /* Other primitives that are not R5RS */
          Random,
          Error,
          NewLock,
          Acquire,
          Release
        ).map(prim => (prim.name, prim)).toMap

        abstract class SingleArgumentPrimWithExp(val name: String) extends Prim:
            def fun(fexp: SchemeFuncall): PartialFunction[Value, Value]

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args match
                case (_, x) :: Nil =>
                  val f = fun(fexp)
                  if f.isDefinedAt(x) then f(x)
                  else signalException(s"$name (${fexp.idn.pos}): invalid argument type $x")
                case _ => signalException(s"$name ($fexp.idn.pos): invalid arguments $args")

        abstract class SingleArgumentPrim(name: String) extends SingleArgumentPrimWithExp(name):
            def fun: PartialFunction[Value, Value]

            def fun(fexp: SchemeFuncall): PartialFunction[Value, Value] = fun

        ////////////////
        // Arithmetic //
        ////////////////
        object Plus extends SimplePrim:
            val name = "+"
            val default: Value = Value.Integer(0)

            def call(args: List[Value], position: Position): Value = args.foldLeft(default)({
              case (Value.Integer(n1), Value.Integer(n2)) => Value.Integer(n1 + n2)
              case (Value.Integer(n1), Value.Real(n2))    => Value.Real(n1 + n2)
              case (Value.Real(n1), Value.Integer(n2))    => Value.Real(n1 + n2)
              case (Value.Real(n1), Value.Real(n2))       => Value.Real(n1 + n2)
              case (x, y)                                 => signalException(s"+ ($position): invalid argument types ($x and $y)")
            })

        object Times extends SimplePrim:
            val name = "*"
            val default: Value = Value.Integer(1)

            def call(args: List[Value], position: Position): Value = args.foldLeft(default)({
              case (Value.Integer(n1), Value.Integer(n2)) => Value.Integer(n1 * n2)
              case (Value.Integer(n1), Value.Real(n2))    => Value.Real(n1 * n2)
              case (Value.Real(n1), Value.Integer(n2))    => Value.Real(n1 * n2)
              case (Value.Real(n1), Value.Real(n2))       => Value.Real(n1 * n2)
              case (x, y)                                 => signalException(s"* ($position): invalid argument types ($x and $y)")
            })

        object Minus extends SimplePrim:
            val name = "-"

            def call(args: List[Value], position: Position) = args match
                case Nil                     => signalException("-: wrong number of arguments")
                case Value.Integer(x) :: Nil => Value.Integer(-x)
                case Value.Real(x) :: Nil    => Value.Real(-x)
                case Value.Integer(x) :: rest =>
                  Plus.call(rest, position) match
                      case Value.Integer(y) => Value.Integer(x - y)
                      case Value.Real(y)    => Value.Real(x - y)
                      case v                => throw new UnexpectedValueTypeException[Value](v)
                case Value.Real(x) :: rest =>
                  Plus.call(rest, position) match
                      case Value.Integer(y) => Value.Real(x - y)
                      case Value.Real(y)    => Value.Real(x - y)
                      case v                => throw new UnexpectedValueTypeException[Value](v)
                case _ => signalException(s"- ($position): invalid arguments $args")

        object Div extends SimplePrim:
            val name = "/"

            def call(args: List[Value], position: Position) = args match
                case Nil                                            => signalException("/: wrong number of arguments")
                case Value.Integer(i) :: Nil if i.equals(BigInt(1)) => Value.Integer(BigInt(1))
                case Value.Integer(x) :: Nil                        => Value.Real(1.0 / x)
                case Value.Real(x) :: Nil                           => Value.Real(1.0 / x)
                case Value.Integer(x) :: rest =>
                  Times.call(rest, position) match
                      case Value.Integer(y) =>
                        if x % y == 0 then Value.Integer(x / y)
                        else Value.Real(x.toDouble / y)
                      case Value.Real(y) => Value.Real(x.doubleValue / y)
                      case v             => throw new UnexpectedValueTypeException[Value](v)
                case Value.Real(x) :: rest =>
                  Times.call(rest, position) match
                      case Value.Integer(y) => Value.Real(x / y)
                      case Value.Real(y)    => Value.Real(x / y)
                      case v                => throw new UnexpectedValueTypeException[Value](v)
                case _ => signalException(s"/ ($position): invalid arguments $args")

        object Modulo extends SimplePrim:
            val name = "modulo"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil if y != 0 =>
                  Value.Integer(maf.lattice.MathOps.modulo(x, y))
                case _ :: _ :: Nil => signalException(s"$name ($position): illegal computation: modulo zero")
                case _             => signalException(s"$name ($position): invalid arguments $args")

        object Abs extends SingleArgumentPrim("abs"):
            def fun =
                case Value.Integer(x) => Value.Integer(x.abs)
                case Value.Real(x)    => Value.Real(scala.math.abs(x))

        abstract class DoublePrim(name: String, f: Double => Double) extends SingleArgumentPrim(name):
            def fun =
                case Value.Real(x)    => Value.Real(f(x))
                case Value.Integer(x) => Value.Real(f(x.toDouble))

        object Sin extends DoublePrim("sin", scala.math.sin)

        object ASin extends DoublePrim("asin", scala.math.asin)

        object Cos extends DoublePrim("cos", scala.math.cos)

        object ACos extends DoublePrim("acos", scala.math.acos)

        object Tan extends DoublePrim("tan", scala.math.tan)

        object ATan extends DoublePrim("atan", scala.math.atan)

        object Log extends DoublePrim("log", scala.math.log)

        object Sqrt extends SingleArgumentPrim("sqrt"):
            def fun =
                case Value.Integer(x) if x < 0 => signalException(s"sqrt: negative argument $x")
                case Value.Real(x) if x < 0    => signalException(s"sqrt: negative argument $x")
                case Value.Integer(x) =>
                  val r = scala.math.sqrt(x.toDouble)
                  if r == r.floor then Value.Integer(r.toInt)
                  else Value.Real(r)
                case Value.Real(x) => Value.Real(scala.math.sqrt(x))

        object Expt extends SimplePrim:
            val name = "expt"

            // TODO: expt should also preserve exactness if possible
            def call(args: List[Value], position: Position): Value = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil =>
                  Value.Integer(scala.math.pow(x.toDouble, y.toDouble).toInt)
                case Value.Integer(x) :: Value.Real(y) :: Nil =>
                  Value.Real(scala.math.pow(x.toDouble, y))
                case Value.Real(x) :: Value.Integer(y) :: Nil =>
                  Value.Real(scala.math.pow(x, y.toDouble))
                case Value.Real(x) :: Value.Real(y) :: Nil =>
                  Value.Real(scala.math.pow(x, y))
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object Ceiling extends SingleArgumentPrim("ceiling"):
            def fun =
                case x: Value.Integer => x
                case Value.Real(x)    => Value.Real(x.ceil)

        object Floor extends SingleArgumentPrim("floor"):
            def fun =
                case x: Value.Integer => x
                case Value.Real(x)    => Value.Real(x.floor)

        object Quotient extends SimplePrim:
            val name = "quotient"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Integer(x / y)
                case _                                           => signalException(s"$name ($position): invalid arguments $args")

        object Remainder extends SimplePrim:
            val name = "remainder"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Integer(x % y)
                case _                                           => signalException(s"$name ($position): invalid arguments $args")

        object Round extends SingleArgumentPrim("round"):
            def fun =
                case x: Value.Integer => x
                case Value.Real(x)    => Value.Real(maf.lattice.MathOps.round(x))

        object Evenp extends SingleArgumentPrim("even?"):
            def fun =
                case Value.Integer(x) if x % 2 == 0 => Value.Bool(true)
                case _: Value.Integer               => Value.Bool(false)

        object Oddp extends SingleArgumentPrim("odd?"):
            def fun =
                case Value.Integer(x) if x % 2 == 1 => Value.Bool(true)
                case _: Value.Integer               => Value.Bool(false)

        object Negativep extends SingleArgumentPrim("negative?"):
            def fun =
                case Value.Integer(x) if x < 0 => Value.Bool(true)
                case _: Value.Integer          => Value.Bool(false)

        object Positivep extends SingleArgumentPrim("positive?"):
            def fun =
                case Value.Integer(x) if x > 0 => Value.Bool(true)
                case _: Value.Integer          => Value.Bool(false)

        object Zerop extends SingleArgumentPrim("zero?"):
            def fun =
                case Value.Integer(i) if i.equals(BigInt(0)) => Value.Bool(true)
                case _: Value.Integer                        => Value.Bool(false)

        object Max extends SimplePrim:
            val name = "max"

            def max(maximum: Value, rest: List[Value]): Value = rest match
                case Nil => maximum
                case x :: rest =>
                  max(
                    x match {
                      case Value.Integer(n1) =>
                        maximum match {
                          case Value.Integer(n2) =>
                            if n1 > n2 then {
                              Value.Integer(n1)
                            } else {
                              maximum
                            }
                          case Value.Real(n2) =>
                            val r = n1.toDouble
                            if r > n2 then {
                              Value.Real(r)
                            } else {
                              maximum
                            }
                          case v => throw new UnexpectedValueTypeException[Value](v)
                        }
                      case Value.Real(n1) =>
                        maximum match {
                          case Value.Integer(n2) =>
                            val r = n2.toDouble
                            if n1 > r then {
                              Value.Real(n1)
                            } else {
                              maximum
                            }
                          case Value.Real(n2) =>
                            if n1 > n2 then {
                              Value.Real(n1)
                            } else {
                              Value.Real(n2)
                            }
                          case v => throw new UnexpectedValueTypeException[Value](v)
                        }
                      case v => throw new UnexpectedValueTypeException[Value](v)
                    },
                    rest
                  )

            def call(args: List[Value], position: Position): Value = args match
                case Nil => signalException(s"max ($position): wrong number of arguments")
                case Value.Integer(first) :: rest =>
                  max(Value.Integer(first), rest)
                case Value.Real(first) :: rest =>
                  max(Value.Real(first), rest)
                case _ => signalException(s"max ($position): invalid arguments $args")

        object Min extends SimplePrim:
            val name = "min"

            def min(minimum: Value, rest: List[Value]): Value = rest match
                case Nil => minimum
                case x :: rest =>
                  min(
                    x match {
                      case Value.Integer(n1) =>
                        minimum match {
                          case Value.Integer(n2) =>
                            if n1 < n2 then {
                              Value.Integer(n1)
                            } else {
                              minimum
                            }
                          case Value.Real(n2) =>
                            val r = n1.toDouble
                            if r < n2 then {
                              Value.Real(r)
                            } else {
                              minimum
                            }
                          case v => throw new UnexpectedValueTypeException[Value](v)
                        }
                      case Value.Real(n1) =>
                        minimum match {
                          case Value.Integer(n2) =>
                            val r = n2.toDouble
                            if n1 < r then {
                              Value.Real(n1)
                            } else {
                              minimum
                            }
                          case Value.Real(n2) =>
                            if n1 < n2 then {
                              Value.Real(n1)
                            } else {
                              Value.Real(n2)
                            }
                          case v => throw new UnexpectedValueTypeException[Value](v)
                        }
                      case v => throw new UnexpectedValueTypeException[Value](v)
                    },
                    rest
                  )

            def call(args: List[Value], position: Position): Value = args match
                case Nil => signalException(s"min ($position): wrong number of arguments")
                case Value.Integer(first) :: rest =>
                  min(Value.Integer(first), rest)
                case Value.Real(first) :: rest =>
                  min(Value.Real(first), rest)
                case _ => signalException(s"min ($position): invalid arguments $args")

        object Gcd extends SimplePrim:
            val name = "gcd"

            def gcd(a: BigInt, b: BigInt): BigInt = if b == 0 then a
            else gcd(b, a % b)

            def call(args: List[Value], position: Position): Value.Integer = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Integer(gcd(x, y))
                case _                                           => signalException(s"gcd ($position): invalid arguments $args")

        object LessThan extends SimplePrim:
            val name = "<"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x < y)
                case Value.Integer(x) :: Value.Real(y) :: Nil    => Value.Bool(x < y)
                case Value.Real(x) :: Value.Integer(y) :: Nil    => Value.Bool(x < y)
                case Value.Real(x) :: Value.Real(y) :: Nil       => Value.Bool(x < y)
                case _                                           => signalException(s"< ($position): invalid arguments $args")

        object LessOrEqual extends SimplePrim:
            val name = "<="

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x <= y)
                case Value.Integer(x) :: Value.Real(y) :: Nil    => Value.Bool(x <= y)
                case Value.Real(x) :: Value.Integer(y) :: Nil    => Value.Bool(x <= y)
                case Value.Real(x) :: Value.Real(y) :: Nil       => Value.Bool(x <= y)
                case _                                           => signalException(s"<= ($position): invalid arguments $args")

        object GreaterThan extends SimplePrim:
            val name = ">"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x > y)
                case Value.Integer(x) :: Value.Real(y) :: Nil    => Value.Bool(x > y)
                case Value.Real(x) :: Value.Integer(y) :: Nil    => Value.Bool(x > y)
                case Value.Real(x) :: Value.Real(y) :: Nil       => Value.Bool(x > y)
                case _                                           => signalException(s"$name ($position): invalid arguments $args")

        object GreaterOrEqual extends SimplePrim:
            val name = ">="

            def call(args: List[Value], position: Position) = args match
                case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x >= y)
                case Value.Integer(x) :: Value.Real(y) :: Nil    => Value.Bool(x >= y)
                case Value.Real(x) :: Value.Integer(y) :: Nil    => Value.Bool(x >= y)
                case Value.Real(x) :: Value.Real(y) :: Nil       => Value.Bool(x >= y)
                case _                                           => signalException(s"$name ($position): invalid arguments $args")

        object NumEq extends SimplePrim:
            val name = "="

            @scala.annotation.tailrec
            def numEqInt(first: BigInt, l: List[Value]): Value = l match
                case Nil                                    => Value.Bool(true)
                case Value.Integer(x) :: rest if x == first => numEqInt(first, rest)
                case (_: Value.Integer) :: _                => Value.Bool(false)
                case Value.Real(x) :: rest if x == first    => numEqInt(first, rest)
                case (_: Value.Real) :: _                   => Value.Bool(false)
                case _                                      => signalException(s"=: invalid type of arguments $l")

            @scala.annotation.tailrec
            def numEqReal(first: Double, l: List[Value]): Value = l match
                case Nil                                    => Value.Bool(true)
                case Value.Integer(x) :: rest if x == first => numEqReal(first, rest)
                case (_: Value.Integer) :: _                => Value.Bool(false)
                case Value.Real(x) :: rest if x == first    => numEqReal(first, rest)
                case (_: Value.Real) :: _                   => Value.Bool(false)
                case _                                      => signalException(s"=: invalid type of arguments $l")

            def call(args: List[Value], position: Position): Value = args match
                case Nil                      => Value.Bool(true)
                case Value.Integer(x) :: rest => numEqInt(x, rest)
                case Value.Real(x) :: rest    => numEqReal(x, rest)
                case _                        => signalException(s"$name ($position): invalid type of arguments $args")

        //////////////
        // Booleans //
        //////////////
        object Not extends SingleArgumentPrim("not"):
            def fun =
                case Value.Bool(b) => Value.Bool(!b)
                case _             => Value.Bool(false) /* any non-bool value is considered true */

        /////////////////
        // Conversions //
        /////////////////
        object ExactToInexact extends SingleArgumentPrim("exact->inexact"):
            def fun =
                case Value.Integer(x) => Value.Real(x.toDouble)
                case x: Value.Real    => x

        object InexactToExact extends SingleArgumentPrim("inexact->exact"):
            def fun =
                case x: Value.Integer => x
                case Value.Real(x)    => Value.Integer(x.toInt) /* TODO: fractions */

        object NumberToString extends SingleArgumentPrimWithExp("number->string"):
            def fun(fexp: SchemeFuncall) =
                case Value.Integer(x) => allocateStr(fexp, s"$x")
                case Value.Real(x)    => allocateStr(fexp, s"$x")

        object SymbolToString extends SingleArgumentPrimWithExp("symbol->string"):
            def fun(fexp: SchemeFuncall) = { case Value.Symbol(x) =>
              allocateStr(fexp, x)
            }

        object StringToSymbol extends SingleArgumentPrim("string->symbol"):
            def fun = { case Value.Pointer(addr) =>
              Value.Symbol(getString(addr))
            }

        object CharToInteger extends SingleArgumentPrim("char->integer"):
            def fun = { case Value.Character(c) =>
              Value.Integer(c.toInt)
            }

        object CharToString extends SingleArgumentPrimWithExp("char->string"):
            def fun(fexp: SchemeFuncall) = { case Value.Character(c) =>
              allocateStr(fexp, c.toString)
            }

        object IntegerToChar extends SingleArgumentPrim("integer->char"):
            def fun = { case Value.Integer(n) =>
              Value.Character(n.toChar)
            }

        ////////
        // IO //
        ////////
        object `input-port?` extends SingleArgumentPrim("input-port?"):
            def fun =
                case _: Value.InputPort => Value.Bool(true)
                case _                  => Value.Bool(false)

        object `output-port?` extends SingleArgumentPrim("output-port?"):
            def fun =
                case _: Value.OutputPort => Value.Bool(true)
                case _                   => Value.Bool(false)

        object `eof-object?` extends SingleArgumentPrim("eof-object?"):
            def fun =
                case Value.EOF => Value.Bool(true)
                case _         => Value.Bool(false)

        object `open-input-file` extends SingleArgumentPrim("open-input-file"):
            def fun = { case Value.Pointer(addr) =>
              val str = getString(addr)
              Value.InputPort(io.open(str))
            }

        object `open-output-file` extends SingleArgumentPrim("open-output-file"):
            def fun = { case Value.Pointer(addr) =>
              val str = getString(addr)
              Value.OutputPort(io.open(str))
            }

        object `open-input-string` extends SingleArgumentPrim("open-input-string"):
            def fun = { case Value.Pointer(addr) =>
              val str = getString(addr)
              Value.InputPort(io.fromString(str))
            }

        object `close-input-port` extends SingleArgumentPrim("close-input-port"):
            def fun = { case Value.InputPort(handle) =>
              io.close(handle)
              Value.Undefined(Identity.none)
            }

        object `close-output-port` extends SingleArgumentPrim("close-output-port"):
            def fun = { case Value.OutputPort(handle) =>
              io.close(handle)
              Value.Undefined(Identity.none)
            }

        object `current-input-port` extends SimplePrim:
            val name = "current-input-port"

            def call(args: List[Value], position: Position) = args match
                case Nil => Value.InputPort(io.console)
                case _   => signalException(s"$name ($position): wrong number of arguments, 0 expected, got ${args.length}")

        object `current-output-port` extends SimplePrim:
            val name = "current-output-port"

            def call(args: List[Value], position: Position) = args match
                case Nil => Value.OutputPort(io.console)
                case _   => signalException(s"$name ($position): wrong number of arguments, 0 expected, got ${args.length}")

        class DisplayLike(val name: String) extends SimplePrim:
            def call(args: List[Value], position: Position) = args match
                case v :: Nil =>
                  io.writeString(v.toString, io.console)
                  Value.Undefined(Identity.none)
                case v :: Value.OutputPort(port) :: Nil =>
                  io.writeString(v.toString, port)
                  Value.Undefined(Identity.none)
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object `display` extends DisplayLike("display")

        object `write` extends DisplayLike("write")

        object `write-char` extends SimplePrim:
            val name = "write-char"

            def call(args: List[Value], position: Position) = args match
                case Value.Character(c) :: Nil =>
                  io.writeChar(c, io.console)
                  Value.Undefined(Identity.none)
                case Value.Character(c) :: Value.OutputPort(port) :: Nil =>
                  io.writeChar(c, port)
                  Value.Undefined(Identity.none)
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object `newline` extends SimplePrim:
            val name = "newline"

            def call(args: List[Value], position: Position) = args match
                case Nil =>
                  io.writeString("\n", io.console);
                  Value.Undefined(Identity.none)
                case Value.OutputPort(port) :: Nil =>
                  io.writeString("\n", port)
                  Value.Undefined(Identity.none)
                case _ => signalException(s"$name ($position): wrong number of arguments, 0 expected, got ${args.length}")

        object `read` extends Prim:
            val name = "read"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args match
                case Nil =>
                  io.read(io.console).map(sexp => evalSExp(sexp, fexp)).getOrElse(Value.EOF)
                case (_, Value.InputPort(port)) :: Nil =>
                  io.read(port).map(sexp => evalSExp(sexp, fexp)).getOrElse(Value.EOF)
                case _ => signalException(s"$name (${fexp.idn.pos}): wrong number of arguments, 0 or 1 expected, got ${args.length}")

        object `read-char` extends SimplePrim:
            val name = "read-char"

            def call(args: List[Value], position: Position) = args match
                case Nil =>
                  io.readChar(io.console)
                case Value.InputPort(port) :: Nil =>
                  io.readChar(port)
                case _ => signalException(s"$name ($position): wrong number of arguments, 0 or 1 expected, got ${args.length}")

        object `peek-char` extends SimplePrim:
            val name = "peek-char"

            def call(args: List[Value], position: Position) = args match
                case Nil =>
                  io.peekChar(io.console)
                case Value.InputPort(port) :: Nil =>
                  io.peekChar(port)
                case _ => signalException(s"$name ($position): wrong number of arguments, 0 or 1 expected, got ${args.length}")

        object Error extends SimplePrim:
            val name = "error"

            def call(args: List[Value], position: Position) = signalException(s"user-raised error ($position): $args")

        /////////////////
        // Type checks //
        /////////////////
        object Booleanp extends SingleArgumentPrim("boolean?"):
            def fun =
                case _: Value.Bool => Value.Bool(true)
                case _             => Value.Bool(false)

        object Charp extends SingleArgumentPrim("char?"):
            def fun =
                case _: Value.Character => Value.Bool(true)
                case _                  => Value.Bool(false)

        object Nullp extends SingleArgumentPrim("null?"):
            def fun =
                case Value.Nil => Value.Bool(true)
                case _         => Value.Bool(false)

        object Pairp extends SingleArgumentPrim("pair?"):
            def fun =
                case Value.Pointer(addr) =>
                  lookupStore(addr) match
                      case _: Value.Cons => Value.Bool(true)
                      case _             => Value.Bool(false)
                case _ => Value.Bool(false)

        object Symbolp extends SingleArgumentPrim("symbol?"):
            def fun =
                case _: Value.Symbol => Value.Bool(true)
                case _               => Value.Bool(false)

        object Stringp extends SingleArgumentPrim("string?"):
            def fun =
                case Value.Pointer(addr) =>
                  lookupStore(addr) match
                      case Value.Str(_) => Value.Bool(true)
                      case _            => Value.Bool(false)
                case _ => Value.Bool(false)

        object Integerp extends SingleArgumentPrim("integer?"):
            def fun =
                case _: Value.Integer => Value.Bool(true)
                case _                => Value.Bool(false)

        object Realp extends SingleArgumentPrim("real?"):
            def fun =
                case _: Value.Real    => Value.Bool(true)
                case _: Value.Integer => Value.Bool(true)
                case _                => Value.Bool(false)

        object Numberp extends SingleArgumentPrim("number?"):
            def fun =
                case _: Value.Integer => Value.Bool(true)
                case _: Value.Real    => Value.Bool(true)
                case _                => Value.Bool(false)

        object Vectorp extends SingleArgumentPrim("vector?"):
            def fun =
                case Value.Pointer(a) =>
                  lookupStore(a) match
                      case _: Value.Vector => Value.Bool(true)
                      case _               => Value.Bool(false)
                case _ => Value.Bool(false)

        object Procp extends SingleArgumentPrim("procedure?"):
            def fun =
                case _: Value.Clo       => Value.Bool(true)
                case _: Value.Primitive => Value.Bool(true)
                case _                  => Value.Bool(false)

        /////////////
        // Strings //
        /////////////
        object StringAppend extends Prim:
            val name = "string-append"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value =
              allocateStr(
                fexp,
                args.foldLeft("")((acc, v) =>
                  v match {
                    case (_, Value.Pointer(x)) =>
                      val str = getString(x)
                      s"$acc$str"
                    case _ => signalException(s"$name (${fexp.idn.pos}): invalid argument $v")
                  }
                )
              )

        object MakeString extends Prim:
            val name = "make-string"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args match
                case (_, Value.Integer(length)) :: Nil                            => allocateStr(fexp, "\u0000" * bigIntToInt(length))
                case (_, Value.Integer(length)) :: (_, Value.Character(c)) :: Nil => allocateStr(fexp, c.toString * bigIntToInt(length))
                case _ => signalException(s"$name (${fexp.idn.pos}): invalid arguments $args")

        object StringLength extends SingleArgumentPrim("string-length"):
            def fun = { case Value.Pointer(addr) =>
              val str = getString(addr)
              Value.Integer(str.length)
            }

        object StringRef extends SimplePrim:
            val name = "string-ref"

            def call(args: List[Value], position: Position): Value.Character = args match
                case Value.Pointer(addr) :: Value.Integer(n) :: Nil =>
                  val str = getString(addr)
                  if 0 <= n && n < str.size then Value.Character(str(bigIntToInt(n)))
                  else signalException(s"$name ($position): index out of range")
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object StringSet extends SimplePrim:
            val name = "string-set!"

            def call(args: List[Value], position: Position): Value.Undefined = args match
                case Value.Pointer(addr) :: Value.Integer(idx) :: Value.Character(chr) :: Nil =>
                  val str = getString(addr)
                  if 0 <= idx && idx < str.size then
                      val updatedStr = str.updated(idx.toInt, chr)
                      extendStore(addr, Value.Str(updatedStr))
                      Value.Undefined(Identity.none)
                  else signalException(s"$name ($position): index out of range")
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object StringLt extends SimplePrim:
            val name = "string<?"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Pointer(a1) :: Value.Pointer(a2) :: Nil =>
                  val str1 = getString(a1)
                  val str2 = getString(a2)
                  Value.Bool(str1 < str2)
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object StringToNumber extends SingleArgumentPrim("string->number"):
            def fun = { case Value.Pointer(addr) =>
              val str = getString(addr)
              if str.toIntOption.nonEmpty then Value.Integer(str.toIntOption.get)
              else signalException(s"$name: $str can not be converted into a number")
            }

        object Substring extends Prim:
            val name = "substring"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]) = args match
                case (_, Value.Pointer(a)) :: (_, Value.Integer(from)) :: (_, Value.Integer(to)) :: Nil if from <= to =>
                  val str = getString(a)
                  if 0 <= from && to <= str.size then allocateStr(fexp, str.substring(bigIntToInt(from), bigIntToInt(to)).nn)
                  else signalException(s"$name (${fexp.idn.pos}): indices $from and $to are out of range")
                case _ => signalException(s"$name (${fexp.idn.pos}): invalid arguments $args")

        ///////////////
        // Equality //
        //////////////

        object Eq extends SimplePrim:
            val name = "eq?"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case x :: y :: Nil => Value.Bool(x == y)
                case _             => signalException(s"$name ($position): wrong number of arguments ${args.length}")

        /////////////
        // Vectors //
        /////////////
        object Vector extends Prim:
            val name = "vector"

            def newVector(
                fexp: SchemeFuncall,
                siz: BigInt,
                elms: Map[BigInt, Value],
                ini: Value
              ): Value =
                val ptr = newAddr(AddrInfo.PtrAddr(fexp))
                val vct = Value.Vector(siz, elms, ini)
                extendStore(ptr, vct)
                Value.Pointer(ptr)

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value =
                val elms = args.map(_._2).zipWithIndex.map({ case (e, i) => (BigInt(i), e) }).toMap
                newVector(fexp, args.size, elms, Value.Undefined(fexp.idn))

        object MakeVector extends Prim:
            val name = "make-vector"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args.map(_._2) match
                case Value.Integer(size) :: Nil =>
                  Vector.newVector(fexp, size, Map(), Value.Undefined(fexp.idn))
                case Value.Integer(size) :: init :: Nil =>
                  Vector.newVector(fexp, size, Map(), init)
                case _ => signalException(s"$name (${fexp.idn.pos}): invalid arguments $args")

        object VectorLength extends SingleArgumentPrim("vector-length"):
            def fun = { case Value.Pointer(a) =>
              lookupStore(a) match
                  case Value.Vector(siz, _, _) => Value.Integer(siz)
                  case v                       => throw new UnexpectedValueTypeException[Value](v)
            }

        object VectorRef extends SimplePrim:
            val name = "vector-ref"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Pointer(a) :: Value.Integer(idx) :: Nil =>
                  lookupStore(a) match
                      case Value.Vector(siz, els, ini) if idx >= 0 && idx < siz => els.getOrElse(idx, ini)
                      case Value.Vector(siz, _, _) => signalException(s"$name ($position): index $idx out of range (valid range: [0,${siz - 1}])")
                      case v                       => throw new Exception(s"Vector expected; found $v")
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object VectorSet extends SimplePrim:
            val name = "vector-set!"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Pointer(a) :: Value.Integer(idx) :: v :: Nil =>
                  lookupStore(a) match
                      case Value.Vector(siz, els, ini) if idx >= 0 && idx < siz =>
                        val updatedVct = Value.Vector(siz, els + (idx -> v), ini)
                        extendStore(a, updatedVct)
                        Value.Undefined(Identity.none)
                      case Value.Vector(siz, _, _) => signalException(s"$name ($position): index $idx out of range (valid range: [0,${siz - 1}])")
                      case v                       => throw new Exception(s"Vector expected; found $v")
                case _ => signalException(s"$name ($position): invalid arguments $args")

        //////////
        // Cons //
        //////////

        object Car extends SingleArgumentPrim("car"):
            def fun = { case Value.Pointer(addr) =>
              lookupStore(addr) match
                  case Value.Cons(car, _) => car
                  case v                  => throw new UnexpectedValueTypeException[Value](v)
            }

        object Cdr extends SingleArgumentPrim("cdr"):
            def fun = { case Value.Pointer(addr) =>
              lookupStore(addr) match
                  case Value.Cons(_, cdr) => cdr
                  case v                  => throw new UnexpectedValueTypeException[Value](v)
            }

        object Cons extends Prim:
            val name = "cons"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args match
                case (_, car) :: (_, cdr) :: Nil =>
                  allocateCons(fexp, car, cdr)
                case _ => signalException(s"cons: wrong number of arguments $args")

        object SetCar extends SimplePrim:
            val name = "set-car!"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Pointer(addr) :: v :: Nil =>
                  lookupStore(addr) match
                      case Value.Cons(_, cdr) =>
                        extendStore(addr, Value.Cons(v, cdr))
                        Value.Undefined(Identity.none)
                      case v => throw new UnexpectedValueTypeException[Value](v)
                case _ => signalException(s"$name ($position): invalid arguments $args")

        object SetCdr extends SimplePrim:
            val name = "set-cdr!"

            def call(args: List[Value], position: Position): Value = args match
                case Value.Pointer(addr) :: v :: Nil =>
                  lookupStore(addr) match
                      case Value.Cons(car, _) =>
                        extendStore(addr, Value.Cons(car, v))
                        Value.Undefined(Identity.none)
                      case v => throw new UnexpectedValueTypeException[Value](v)
                case _ => signalException(s"$name ($position): invalid arguments $args")

        ///////////
        // Lists //
        ///////////
        object ListPrim extends Prim:
            val name = "list"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args match
                case Nil => Value.Nil
                case (exp, head) :: rest =>
                  allocateCons(exp, head, call(fexp, rest))

        ///////////
        // Other //
        ///////////
        object Random extends SingleArgumentPrim("random"):
            def fun =
                case Value.Integer(x) => Value.Integer(MathOps.random(x))
                case Value.Real(x)    => Value.Real(scala.math.random() * x)

        object CharEq extends SimplePrim:
            val name = "char=?"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Character(c1) :: Value.Character(c2) :: Nil => Value.Bool(c1 == c2)
                case _                                                 => signalException(s"$name ($position): invalid arguments $args")

        object CharCIEq extends SimplePrim:
            val name = "char-ci=?"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Character(c1) :: Value.Character(c2) :: Nil => Value.Bool(c1.toLower == c2.toLower)
                case _                                                 => signalException(s"$name ($position): invalid arguments $args")

        object CharLt extends SimplePrim:
            val name = "char<?"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Character(c1) :: Value.Character(c2) :: Nil => Value.Bool(c1 < c2)
                case _                                                 => signalException(s"$name ($position): invalid arguments $args")

        object CharCILt extends SimplePrim:
            val name = "char-ci<?"

            def call(args: List[Value], position: Position): Value.Bool = args match
                case Value.Character(c1) :: Value.Character(c2) :: Nil => Value.Bool(c1.toLower < c2.toLower)
                case _                                                 => signalException(s"$name ($position): invalid arguments $args")

        ///////////
        // Locks //
        ///////////

        object NewLock extends Prim:
            val name = "new-lock"

            def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = args match
                case Nil =>
                  val addr = newAddr(AddrInfo.PtrAddr(fexp))
                  val lock = Value.Lock(new java.util.concurrent.locks.ReentrantLock())
                  extendStore(addr, lock)
                  Value.Pointer(addr)
                case _ => signalException(s"new-lock: invalid arguments $args")

        case object Acquire extends SingleArgumentPrim("acquire"):
            def fun = { case Value.Pointer(ptr) =>
              lookupStore(ptr) match
                  case Value.Lock(lck) =>
                    lck.lock()
                    Value.Undefined(Identity.none)
                  case v => throw new UnexpectedValueTypeException[Value](v)
            }

        case object Release extends SingleArgumentPrim("release"):
            def fun = { case Value.Pointer(ptr) =>
              lookupStore(ptr) match
                  case Value.Lock(lck) =>
                    lck.unlock()
                    Value.Undefined(Identity.none)
                  case v => throw new UnexpectedValueTypeException[Value](v)
            }
