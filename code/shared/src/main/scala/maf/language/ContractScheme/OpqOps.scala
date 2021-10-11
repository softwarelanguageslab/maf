package maf.language.ContractScheme

import maf.language.scheme.lattices.{SchemeLattice, SchemeOp}
import maf.language.ContractScheme.ContractValues.Opq
import maf.core.Address

/**
 * This object defines the signatures for the native Scheme functions. The reason for this is that it can automatically resolve operations involving
 * OPQs this way
 */
object OpqOps:
    trait Tpy:
        def ==>:(other: Tpy): Tpy =
          ArrowTpy(other, this)

    /**
     * Everything can be expressed as an uncurried function type,
     *
     * B -uncurry-> B'
     * ------------------- uncurry_arrow A => B -uncurry-> Uncurried { domains = A :: B'.domains, range = B'range }
     *
     * A is a native type
     * --------------------- uncurry_native A -uncurry-> A
     */

    def uncurry(tpy: Tpy): Uncurried =
      tpy match
          case ArrowTpy(domain, range) =>
            val rangeUncurried = uncurry(range)
            Uncurried(domains = domain :: rangeUncurried.domains, rangeUncurried.range)
          case _ =>
            Uncurried(domains = List(), tpy)

    case class Uncurried(domains: List[Tpy], range: Tpy)

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
      Real -> SchemeOp.IsReal,
      Boolean -> SchemeOp.IsBoolean,
      Integer -> SchemeOp.IsInteger,
      String -> SchemeOp.IsString,
      // Pair -> SchemeOp.IsPair,
      Symbol -> SchemeOp.IsSymbol,
      Nil -> SchemeOp.IsNull,
      Any -> SchemeOp.IsAny,
      Vector -> SchemeOp.IsVector
    )

    /** Injection functions for the native types */
    def inject[V: Lat](tpy: Tpy): V = tpy match
        case Real    => Lat[V].realTop
        case Boolean => Lat[V].boolTop
        case Integer => Lat[V].numTop
        case String  => Lat[V].stringTop
        case Symbol  => Lat[V].symbolTop
        case Nil     => Lat[V].nil
        case Any     => Lat[V].opq(Opq())

    /** Signatures for each native */
    val signatures = Map(
      "modulo" -> (Real ==>: Real),
      "*" -> (VarArg(Real) ==>: Real),
      "+" -> (VarArg(Real) ==>: Real),
      "-" -> (VarArg(Real) ==>: Real),
      "/" -> (VarArg(Real) ==>: Real),
      "acos" -> (Real ==>: Real),
      "asin" -> (Real ==>: Real),
      "aton" -> (Real ==>: Real),
      "boolean?" -> (Any ==>: Boolean),
      "true?" -> (Any ==>: Boolean),
      "false?" -> (Any ==>: Boolean),
      "call/cc" -> Unsupported,
      "car" -> (Pair ==>: Any),
      "cdr" -> (Pair ==>: Any),
      "ceiling" -> (Real ==>: Real),
      "char->integer" -> (Char ==>: Integer),
      "char->string" -> (Char ==>: String),
      "char-ci<?" -> (Char ==>: Char ==>: Boolean),
      "char-ci=?" -> (Char ==>: Char ==>: Boolean),
      "char-downcase" -> (Char ==>: Char),
      "char-lower-case?" -> (Char ==>: Boolean),
      "char-upcase" -> (Char ==>: Char),
      "char-upper-case?" -> (Char ==>: Boolean),
      "char<?" -> (Char ==>: Boolean),
      "char=?" -> (Char ==>: Boolean),
      "char?" -> (Any ==>: Boolean),
      "cons" -> (Any ==>: Any ==>: Pair),
      "cos" -> (Real ==>: Real),
      "eq?" -> (Any ==>: Any ==>: Boolean),
      "exact->inexact" -> (Real ==>: Real),
      "expt" -> (Real ==>: Real ==>: Real),
      "floor" -> (Real ==>: Real),
      "inexact->exact" -> (Real ==>: Real),
      "integer->char" -> (Integer ==>: Char),
      "integer?" -> (Any ==>: Boolean),
      "log" -> (Real ==>: Real),
      "make-string" -> (VarArg(Char) ==>: String),
      "null?" -> (Any ==>: Boolean),
      "number->string" -> (Real ==>: String),
      "number?" -> (Any ==>: Boolean),
      "pair?" -> (Any ==>: Boolean),
      "procedure?" -> (Any ==>: Boolean),
      "quotient" -> (Integer ==>: Integer ==>: Integer),
      "real?" -> (Any ==>: Boolean),
      "remainder" -> (Integer ==>: Integer ==>: Integer),
      "round" -> (Real ==>: Integer),
      "set-car!" -> (Pair ==>: Any ==>: Nil),
      "set-cdr!" -> (Pair ==>: Any ==>: Nil),
      "sin" -> (Real ==>: Real),
      "sqrt" -> (Real ==>: Real),
      "string->number" -> (String ==>: Number),
      "string->symbol" -> (String ==>: Symbol),
      "string-append" -> (String ==>: String ==>: String),
      "string-length" -> (String ==>: Integer),
      "string-ref" -> (String ==>: Integer ==>: Char),
      "string-set!" -> (String ==>: Integer ==>: Char ==>: Nil),
      "string<?" -> (String ==>: String ==>: Boolean),
      "string?" -> (Any ==>: Boolean),
      "substring" -> (String ==>: String ==>: Integer ==>: String),
      "symbol->string" -> (Symbol ==>: String),
      "symbol?" -> (Any ==>: Boolean),
      "tan" -> (Real ==>: Real),
      "make-vector" -> (Real ==>: Optional(Integer) ==>: Vector),
      "vector" -> (VarArg(Any) ==>: Vector),
      "vector-length" -> (Vector ==>: Integer),
      "vector-ref" -> (Vector ==>: Integer ==>: Any),
      "vector-set!" -> (Vector ==>: Integer ==>: Any ==>: Nil),
      "vector?" -> (Any ==>: Boolean),
      "<" -> (Real ==>: Real ==>: Boolean),
      "=" -> (Real ==>: Real ==>: Boolean),
      "input-port?" -> (Any ==>: Boolean),
      "output-port?" -> (Any ==>: Boolean),
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

    /** Type alias for SchemeLattice[V, Address] */
    trait Lat[V] extends SchemeLattice[V, Address]
    object Lat:
        def apply[V](using Lat[V]): Lat[V] = summon[Lat[V]]
    // blanket implementation
    implicit def latInstance[V](using SchemeLattice[V, Address]): Lat[V] = new Lat[V]:
        val lat = summon[SchemeLattice[V, Address]]
        export lat.*

    /** Returns true if the given argument matches the given type */
    def matches[V: Lat](value: V, tpy: Tpy): Boolean =
        val operation = typePredicates(tpy)
        operation match
            case SchemeOp.IsAny => true
            case _ =>
              Lat[V].op(operation)(List(value)).map(Lat[V].isTrue).getOrElse(false) ||
                Lat[V].isOpq(value)

    /** Returns true if the arguments match (both in number *and* type) */
    def checkArgs[V: Lat](values: List[V], tpys: List[Tpy]): Boolean = (values, tpys) match
        case (v :: vs, List(VarArg(tpy))) =>
          (v :: vs).forall(v => matches(v, tpy))
        case (List(), List(Optional(_)) | List()) => true
        case (List(v), List(Optional(tpy)))       => matches(v, tpy)
        case (v :: vs, tpy :: tpys) =>
          matches(v, tpy) && checkArgs(vs, tpys)
        case _ => throw new Exception(s"Unsupported comparison of $values witgh $tpys")

    /* Computations with OPQ values */
    def compute[V: Lat](primName: String, args: List[V]): V =
        val signature = uncurry(signatures(primName))
        if signature.range == Unsupported || !checkArgs(args, signature.domains) then Lat[V].bottom
        else inject(signature.range)

    /** Checks whether the abstract computations can be ran using the semantics of opaque values */
    def eligible[V: Lat](args: List[V]): Boolean =
      args.exists(arg => Lat[V].isOpq(arg))
