package maf.language.scheme.lattices

sealed trait SchemeOp {
  val arity: Int
  val name: String
  def checkArity[L](args: List[L]): Unit =
    if (args.size != arity) {
      // This is a runtime error because a lattice operation is improperly called by the analysis developper
      throw new Exception(s"SchemeOp ${name} expects ${arity} arguments but got ${args.size}")
    }
}

object SchemeOp {

  abstract class SchemeOp1(val name: String) extends SchemeOp {
    val arity = 1
  }

  case object Car extends SchemeOp1("car")
  case object Cdr extends SchemeOp1("cdr")
  case object IsNull extends SchemeOp1("null?")
  case object IsBoolean extends SchemeOp1("bool?")
  case object IsCons extends SchemeOp1("cons?")
  case object IsPointer extends SchemeOp1("pointer?")
  case object IsChar extends SchemeOp1("char?")
  case object IsSymbol extends SchemeOp1("symbol?")
  case object IsString extends SchemeOp1("string?")
  case object IsInteger extends SchemeOp1("integer?")
  case object IsReal extends SchemeOp1("real?")
  case object IsVector extends SchemeOp1("vector?")
  case object IsThread extends SchemeOp1("thread?")
  case object IsLock extends SchemeOp1("lock?")
  case object IsProcedure extends SchemeOp1("proc?")
  case object IsInputPort extends SchemeOp1("input-port?")
  case object IsOutputPort extends SchemeOp1("output-port?")
  case object Not extends SchemeOp1("not")
  case object Ceiling extends SchemeOp1("ceiling")
  case object Floor extends SchemeOp1("floor")
  case object Round extends SchemeOp1("round")
  case object Random extends SchemeOp1("random")
  case object Sqrt extends SchemeOp1("sqrt")
  case object Sin extends SchemeOp1("sin")
  case object ASin extends SchemeOp1("asin")
  case object Cos extends SchemeOp1("cos")
  case object ACos extends SchemeOp1("acos")
  case object Tan extends SchemeOp1("tan")
  case object ATan extends SchemeOp1("atan")
  case object Log extends SchemeOp1("log")
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

  abstract class SchemeOp2(val name: String) extends SchemeOp {
    val arity = 2
  }

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
  case object Eq extends SchemeOp2("eq")
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
    Eq,
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

  abstract class SchemeOp3(val name: String) extends SchemeOp {
    val arity = 3
  }

  case object Substring extends SchemeOp3("substring")
  case object VectorSet extends SchemeOp3("vector-set!")
  case object StringSet extends SchemeOp3("string-set!")

  val ternaryOperators: Iterable[SchemeOp3] = Set(Substring, VectorSet, StringSet)
}
