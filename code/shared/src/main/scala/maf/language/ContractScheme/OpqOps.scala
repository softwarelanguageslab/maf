package maf.language.ContractScheme

import maf.core.Monad
import maf.language.scheme.{SchemeExp, SchemeFuncall}
import maf.language.scheme.lattices.{SchemeLattice, SchemeOp}
import maf.language.ContractScheme.ContractValues.Opq
import maf.core.Address
import maf.language.scheme.primitives.{SchemePrimM, SchemePrimitives}
import maf.language.symbolic.*

/**
 * This object defines the signatures for the native Scheme functions. The reason for this is that it can automatically resolve operations involving
 * OPQs this way
 */
object OpqOps:
    trait Tpy:
        def ==>:(other: Tpy): Tpy =
            ArrowTpy(other, this)

        def |||(other: Tpy): Tpy =
            Union(this, other)

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

    def symbolicContracts(tpy: Tpy, args: List[Unit]): List[SchemeOp] =
        val uncurried = uncurry(tpy)
        def iter(tpys: List[Tpy], args: List[Unit]): List[SchemeOp] = (tpys, args) match
            case (List(VarArg(tpy)), rest) =>
                (0 to rest.size).map(_ => typePredicates(tpy)).toList
            case (List(Optional(tpy)), List(rest)) =>
                List(typePredicates(tpy))
            case (tpy :: restTpys, arg :: restArgs) =>
                typePredicates(tpy) :: iter(restTpys, restArgs)
            case (List(), _ /*List()*/ ) => List()

        iter(uncurried.domains, args)

    case class Uncurried(domains: List[Tpy], range: Tpy):
        def checkArity[V](args: List[V]): Boolean =
            def check(domains: List[Tpy], args: List[V]): Boolean = (domains, args) match
                // if both lists are empty, all arguments have been matched
                case (List(), List()) => true
                // a VarArg may only occur at the end, any number of remaining arguments is accepted for this
                case (List(VarArg(_)), _) => true
                // maximum one element allowed if the end is an optional
                case (List(Optional(_)), List(_) | List()) => true
                case (_ :: rest, _ :: restArg)             => check(rest, restArg)
                case (_, _)                                => false

            check(domains, args)

    /** Represents a type of a function with the specified domain and range */
    case class ArrowTpy(domain: Tpy, range: Tpy) extends Tpy

    /** Variable argument type */
    case class VarArg(tpy: Tpy) extends Tpy

    /* Represents an argument that is optional */
    case class Optional(tpy: Tpy) extends Tpy

    /** Represents the type of an unsupported operation */
    case object Unsupported extends Tpy

    /** Represents a union type, during checking it will try the variants from left to right, and convert opaque values as such if necessary */
    case class Union(a: Tpy, b: Tpy) extends Tpy

    /**
     * Represents a variable that can be used in type signatures.
     *
     * It matches a concrete value if the concrete value matches the bound type. (TODO)
     *
     * @param name
     *   the name of the variable
     * @param boundTo
     *   the type the variable is bound to (if any)
     */
    case class Var(name: String, boundTo: Option[Tpy] = None) extends Tpy:
        def bind(tpy: Tpy): Var =
            Var(name, Some(tpy))

        /** Checks whether the condition is satisfies for the concrete values the variable is bound to */
        def forall(pred: Tpy => Boolean): Tpy = Forall(this, pred)

    /**
     * Matches a concrete value if the concrete value bound at vrr satisfies the given predicate.
     *
     * If the concrete value is a list of values, then the entire list of values should match the type (TODO)
     */
    case class Forall(vrr: Var, pred: Tpy => Boolean) extends Tpy

    /** If-then-else. Matches a concrete value if cns matches the concrete value in case cnd matches, or matches alt if cnd does not match (TODO) */
    case class Ite(cnd: Tpy, cns: Tpy, alt: Tpy) extends Tpy

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
      Pair -> SchemeOp.IsAny, // TODO: should actually use primitive pair? here!
      Symbol -> SchemeOp.IsSymbol,
      Nil -> SchemeOp.IsNull,
      Any -> SchemeOp.IsAny,
      Vector -> SchemeOp.IsVector,
      Char -> SchemeOp.IsChar,
      Union(Integer, Real) -> SchemeOp.SyntheticSchemeOp("number?", 1)
    )

    /** Injection functions for the native and union types */
    def inject[V: Lat](tpy: Tpy): V = tpy match
        case Real        => Lat[V].realTop
        case Boolean     => Lat[V].boolTop
        case Integer     => Lat[V].numTop
        case String      => Lat[V].stringTop
        case Symbol      => Lat[V].symbolTop
        case Nil         => Lat[V].nil
        case Any         => Lat[V].opq(Opq())
        case Char        => Lat[V].charTop
        case Union(a, b) => Lat[V].join(inject(a), inject(b))

    /** A type for a number */
    val NumberTy: Tpy = Integer ||| Real

    /** Signatures for each native */
    val signatures = Map(
      "modulo" -> (Real ==>: Real),
      "*" -> (VarArg(NumberTy) ==>: NumberTy),
      "+" -> (VarArg(NumberTy) ==>: NumberTy),
      "-" -> (VarArg(NumberTy) ==>: NumberTy),
      "/" -> (VarArg(NumberTy) ==>: NumberTy),
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
      "__struct_ref" -> (Any ==>: Integer ==>: Any), // TODO: make this signature more precise
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
      "<" -> (NumberTy ==>: NumberTy ==>: Boolean),
      "=" -> (NumberTy ==>: NumberTy ==>: Boolean),
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
    def matches[V: Lat](value: V, tpy: Tpy): Boolean = tpy match
        case Union(a, b) => matches(value, a) || matches(value, b)
        case _ =>
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

    import maf.core.Monad.MonadSyntaxOps

    /**
     * Returns "true" if the application of the given primitive on the given arguments does not need to be "faked" but can instead be directly
     * executed using one of the SchemeLatticePrimitives
     *
     * @param fexp
     *   the original function call expression
     * @param primName
     *   the name of the primitive to apply
     * @param args
     *   the arguments of the function
     * @param primitives
     *   a map from names of primitives to the primitives themselves, may be used for checking the type of the arguments
     */
    def directlyApplicable[M[_], V: Lat](
        fexp: SchemeExp,
        primName: String,
        args: List[V],
        primitives: SchemePrimitives[V, Address]
      )(using SchemePrimM[M, Address, V]
      ): M[Boolean] = primName match
        case "cons" =>
            // cons is always directly applicable, not matter what the arguments are
            Monad[M].unit(true)

        case "set-car!" | "set-cdr!" =>
            // set-car! and set-cdr! are only applicable if the first argument is a pair
            val isPair = primitives("pair?")
            isPair.call(fexp, List(args(0))) map (Lat[V].isTrue)

        case _ => Monad[M].unit(false)

    def appl[M[_], V: Lat](
        fexp: SchemeExp,
        primName: String,
        args: List[V],
        primitives: SchemePrimitives[V, Address]
      )(using SchemePrimM[M, Address, V]
      ): M[V] =
        primitives(primName).call(fexp, args)

    /* Computations with OPQ values
     *
     * @param primName the name of the primitive to compute
     * @param args the list of arguments (may include opaque values)
     * @param primitives a map from names of primitives to the primitives themselves, are sometimes used when access to the store is required
     */
    def compute[M[_], V: Lat](
        fexp: SchemeExp,
        primName: String,
        args: List[V],
        primitives: SchemePrimitives[V, Address]
      )(using SchemePrimM[M, Address, V]
      ): M[V] =
        val signature = uncurry(signatures(primName))

        // first check whether the number of arguments is correct according to the signature
        if !signature.checkArity(args) then
            println(s"Error in arity of $fexp")
            return Monad[M].unit(Lat[V].bottom) // TODO: use failure op

        Monad.mIf(directlyApplicable(fexp, primName, args, primitives)) /* then */ { appl(fexp, primName, args, primitives) } /* else */ {
            if signature.range == Unsupported || !checkArgs(args, signature.domains) then Monad[M].unit(Lat[V].bottom)
            else Monad[M].unit(inject(signature.range))
        }

    /** Checks whether the abstract computations can be ran using the semantics of opaque values */
    def eligible[V: Lat](args: List[V]): Boolean =
        args.exists(arg => Lat[V].isOpq(arg)) && !args.exists(arg => Lat[V].isBottom(arg))
