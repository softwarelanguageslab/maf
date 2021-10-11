package maf.language.scheme.lattices

sealed trait SchemeOp:
    val arity: Int
    val name: String
    def checkArity[L](args: List[L]): Unit =
      if args.size != arity then
          // This is a runtime error because a lattice operation is improperly called by the analysis developper
          throw new Exception(s"SchemeOp ${name} expects ${arity} arguments but got ${args.size}")

object SchemeOp:
    /** SchemeOpCat specifies a hierarchy of categories (implemented as blank traits used as markers) over the operations. */
    sealed trait SchemeOpCat

    /** Operations that return booleans to represent a predicate on a particular value */
    sealed trait PredicateOp extends SchemeOpCat

    /** Operations that check whether a value is of a particular type */
    sealed trait TypeOp extends PredicateOp

    /** Operations on booleans */
    sealed trait BoolOp extends SchemeOpCat

    /** Operations on reals, also work on integers */
    sealed trait RealOp extends SchemeOpCat

    sealed trait SchemeOp1(val name: String) extends SchemeOp:
        val arity = 1

    case object Car extends SchemeOp1("car")
    case object Cdr extends SchemeOp1("cdr")
    case object IsNull extends SchemeOp1("null?") with PredicateOp
    case object IsBoolean extends SchemeOp1("bool?") with TypeOp
    case object IsAny extends SchemeOp1("any?")
    case object IsTrue extends SchemeOp1("true?") with PredicateOp
    case object IsFalse extends SchemeOp1("false?") with PredicateOp
    case object IsCons extends SchemeOp1("cons?") with TypeOp
    case object IsPointer extends SchemeOp1("pointer?") with TypeOp
    case object IsChar extends SchemeOp1("char?") with TypeOp
    case object IsSymbol extends SchemeOp1("symbol?") with TypeOp
    case object IsString extends SchemeOp1("string?") with TypeOp
    case object IsInteger extends SchemeOp1("integer?") with TypeOp
    case object IsReal extends SchemeOp1("real?") with TypeOp
    case object IsVector extends SchemeOp1("vector?") with TypeOp
    case object IsThread extends SchemeOp1("thread?") with TypeOp
    case object IsLock extends SchemeOp1("lock?") with TypeOp
    case object IsProcedure extends SchemeOp1("proc?") with TypeOp
    case object IsInputPort extends SchemeOp1("input-port?") with TypeOp
    case object IsOutputPort extends SchemeOp1("output-port?") with TypeOp
    case object Not extends SchemeOp1("not") with BoolOp
    case object Ceiling extends SchemeOp1("ceiling") with RealOp
    case object Floor extends SchemeOp1("floor") with RealOp
    case object Round extends SchemeOp1("round") with RealOp
    case object Random extends SchemeOp1("random")
    case object Sqrt extends SchemeOp1("sqrt") with RealOp
    case object Sin extends SchemeOp1("sin") with RealOp
    case object ASin extends SchemeOp1("asin") with RealOp
    case object Cos extends SchemeOp1("cos") with RealOp
    case object ACos extends SchemeOp1("acos") with RealOp
    case object Tan extends SchemeOp1("tan") with RealOp
    case object ATan extends SchemeOp1("atan") with RealOp
    case object Log extends SchemeOp1("log") with RealOp
    case object VectorLength extends SchemeOp1("vector-len")
    case object StringLength extends SchemeOp1("string-len")
    case object NumberToString extends SchemeOp1("number->string")
    case object SymbolToString extends SchemeOp1("symbol->string")
    case object StringToSymbol extends SchemeOp1("string->symbol")
    case object StringToNumber extends SchemeOp1("string->number")
    case object IntegerToCharacter extends SchemeOp1("integer->char")
    case object ExactToInexact extends SchemeOp1("exact->inexact")
    case object InexactToExact extends SchemeOp1("inexact->exact")
    case object CharacterToInteger extends SchemeOp1("char->integer")
    case object CharacterToString extends SchemeOp1("char->string")
    case object CharacterDowncase extends SchemeOp1("char-downcase")
    case object CharacterUpcase extends SchemeOp1("char-upcase")
    case object CharacterIsLower extends SchemeOp1("char-is-lower?")
    case object CharacterIsUpper extends SchemeOp1("char-is-upper?")
    case object MakeInputPort extends SchemeOp1("make-input-port")
    case object MakeOutputPort extends SchemeOp1("make-input-port")

    val unaryOperators: Iterable[SchemeOp1] = Set(
      Car,
      Cdr,
      IsNull,
      IsBoolean,
      IsTrue,
      IsFalse,
      IsCons,
      IsPointer,
      IsChar,
      IsSymbol,
      IsString,
      IsInteger,
      IsReal,
      IsVector,
      IsThread,
      IsLock,
      IsProcedure,
      IsInputPort,
      IsOutputPort,
      Not,
      Ceiling,
      Floor,
      Round,
      Random,
      Sqrt,
      Sin,
      ASin,
      Cos,
      ACos,
      Tan,
      ATan,
      Log,
      VectorLength,
      StringLength,
      NumberToString,
      SymbolToString,
      StringToSymbol,
      StringToNumber,
      IntegerToCharacter,
      ExactToInexact,
      InexactToExact,
      CharacterToInteger,
      CharacterToString,
      CharacterDowncase,
      CharacterUpcase,
      CharacterIsLower,
      CharacterIsUpper,
      MakeInputPort,
      MakeOutputPort
    )

    sealed trait SchemeOp2(val name: String) extends SchemeOp:
        val arity = 2

    case object Plus extends SchemeOp2("+")
    case object Minus extends SchemeOp2("-")
    case object Times extends SchemeOp2("*")
    case object Div extends SchemeOp2("/")
    case object Quotient extends SchemeOp2("quotient")
    case object Modulo extends SchemeOp2("modulo")
    case object Remainder extends SchemeOp2("remainder")
    case object Expt extends SchemeOp2("expt")
    case object Lt extends SchemeOp2("<")
    case object NumEq extends SchemeOp2("=")
    case object MakeString extends SchemeOp2("make-string")
    case object StringAppend extends SchemeOp2("string-append")
    case object StringRef extends SchemeOp2("string-ref")
    case object StringLt extends SchemeOp2("string<")
    case object CharacterEq extends SchemeOp2("char=")
    case object CharacterLt extends SchemeOp2("char<")
    case object CharacterEqCI extends SchemeOp2("char-ci=")
    case object CharacterLtCI extends SchemeOp2("char-ci<")
    case object MakeVector extends SchemeOp2("make-vector")
    case object VectorRef extends SchemeOp2("vector-ref")

    val binaryOperators: Iterable[SchemeOp2] = Set(
      Plus,
      Minus,
      Times,
      Div,
      Quotient,
      Modulo,
      Remainder,
      Expt,
      Lt,
      NumEq,
      MakeString,
      StringAppend,
      StringRef,
      StringLt,
      CharacterEq,
      CharacterLt,
      CharacterEqCI,
      CharacterLtCI,
      MakeVector,
      VectorRef
    )

    sealed trait SchemeOp3(val name: String) extends SchemeOp:
        val arity = 3

    case object Substring extends SchemeOp3("substring")
    case object VectorSet extends SchemeOp3("vector-set!")
    case object StringSet extends SchemeOp3("string-set!")

    val ternaryOperators: Iterable[SchemeOp3] = Set(Substring, VectorSet, StringSet)
