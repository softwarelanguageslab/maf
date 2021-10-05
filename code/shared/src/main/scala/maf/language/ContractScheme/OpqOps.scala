package maf.language.ContractScheme

import maf.language.ContractScheme.OpqOps.Unsupported

/**
 * This object defines the signatures for the native Scheme functions. The reason for this is that it can automatically resolve operations involving
 * OPQs this way
 */
object OpqOps:
    trait Tpy:
        def ==>(other: Tpy): Tpy =
          ArrowTpy(this, other)

    /** Represents a type of a function with the specified domain and range */
    case class ArrowTpy(domain: Tpy, range: Tpy) extends Tpy

    /** Variable argument type */
    case class VarArg(tpy: Tpy) extends Tpy

    /* Represents an argument that is optional */
    case class Optional(tpy: Tpy) extends Tpy

    /** Represents the type of an unsupported operation */
    case object Unsupported extends Tpy

    /** Native types */
    case object Real extends Tpy
    case object Number extends Tpy
    case object Integer extends Tpy
    case object String extends Tpy
    case object Char extends Tpy
    case object Pair extends Tpy
    case object Boolean extends Tpy
    case object Symbol extends Tpy
    case object Nil extends Tpy
    case object Any extends Tpy
    case object Vector extends Tpy

    /** Type predicates for each native type */
    val typePredicates = Map(
      Real -> "real?",
      Boolean -> "boolean?",
      Integer -> "integer?",
      String -> "string?",
      Pair -> "pair?",
      Symbol -> "symbol?",
      Nil -> "null?",
      Any -> "any?",
    )

    /** Signatures for each native */
    val signatures = Map(
      "modulo" -> (Real ==> Real),
      "*" -> (VarArg(Real) ==> Real),
      "+" -> (VarArg(Real) ==> Real),
      "-" -> (VarArg(Real) ==> Real),
      "/" -> (VarArg(Real) ==> Real),
      "acos" -> (Real ==> Real),
      "asin" -> (Real ==> Real),
      "aton" -> (Real ==> Real),
      "boolean?" -> (Any ==> Boolean),
      "true?" -> (Any ==> Boolean),
      "false?" -> (Any ==> Boolean),
      "call/cc" -> Unsupported,
      "car" -> (Pair ==> Any),
      "cdr" -> (Pair ==> Any),
      "ceiling" -> (Real ==> Real),
      "char->integer" -> (Char ==> Integer),
      "char->string" -> (Char ==> String),
      "char-ci<?" -> (Char ==> Char ==> Boolean),
      "char-ci=?" -> (Char ==> Char ==> Boolean),
      "char-downcase" -> (Char ==> Char),
      "char-lower-case?" -> (Char ==> Boolean),
      "char-upcase" -> (Char ==> Char),
      "char-upper-case?" -> (Char ==> Boolean),
      "char<?" -> (Char ==> Boolean),
      "char=?" -> (Char ==> Boolean),
      "char?" -> (Any ==> Boolean),
      "cons" -> (Any ==> Any ==> Pair),
      "cos" -> (Real ==> Real),
      "eq?" -> (Any ==> Any ==> Boolean),
      "exact->inexact" -> (Real ==> Real),
      "expt" -> (Real ==> Real ==> Real),
      "floor" -> (Real ==> Real),
      "inexact->exact" -> (Real ==> Real),
      "integer->char" -> (Integer ==> Char),
      "integer?" -> (Any ==> Boolean),
      "log" -> (Real ==> Real),
      "make-string" -> (VarArg(Char) ==> String),
      "null?" -> (Any ==> Boolean),
      "number->string" -> (Real ==> String),
      "number?" -> (Any ==> Boolean),
      "pair?" -> (Any ==> Boolean),
      "procedure?" -> (Any ==> Boolean),
      "quotient" -> (Integer ==> Integer ==> Integer),
      "real?" -> (Any ==> Boolean),
      "remainder" -> (Integer ==> Integer ==> Integer),
      "round" -> (Real ==> Integer),
      "set-car!" -> (Pair ==> Any ==> Nil),
      "set-cdr!" -> (Pair ==> Any ==> Nil),
      "sin" -> (Real ==> Real),
      "sqrt" -> (Real ==> Real),
      "string->number" -> (String ==> Number),
      "string->symbol" -> (String ==> Symbol),
      "string-append" -> (String ==> String ==> String),
      "string-length" -> (String ==> Integer),
      "string-ref" -> (String ==> Integer ==> Char),
      "string-set!" -> (String ==> Integer ==> Char ==> Nil),
      "string<?" -> (String ==> String ==> Boolean),
      "string?" -> (Any ==> Boolean),
      "substring" -> (String ==> String ==> Integer ==> String),
      "symbol->string" -> (Symbol ==> String),
      "symbol?" -> (Any ==> Boolean),
      "tan" -> (Real ==> Real),
      "make-vector" -> (Real ==> Optional(Integer) ==> Vector),
      "vector" -> (VarArg(Any) ==> Vector),
      "vector-length" -> (Vector ==> Integer),
      "vector-ref" -> (Vector ==> Integer ==> Any),
      "vector-set!" -> (Vector ==> Integer ==> Any ==> Nil),
      "vector?" -> (Any ==> Boolean),
      "<" -> (Real ==> Real ==> Boolean),
      "=" -> (Real ==> Real ==> Boolean),
      "input-port?" -> (Any ==> Boolean),
      "output-port?" -> (Any ==> Boolean),
      "open-input-file" -> Unsupported,
      "open-input-string" -> Unsupported,
      "open-output-file" -> Unsupported,
      "close-input-port" -> Unsupported,
      "close-output-port" -> Unsupported,
      "current-input-port" -> Unsupported,
      "current-output-port" -> Unsupported,
      "read-char" -> Unsupported,
      "peek-char" -> Unsupported,
      "write-char" -> Unsupported,
      "read" -> Unsupported,
      "write" -> Unsupported,
      "display" -> Unsupported,
      "eof-object?" -> Unsupported,
      "random" -> (Real),
      "error" -> Unsupported,
    )
