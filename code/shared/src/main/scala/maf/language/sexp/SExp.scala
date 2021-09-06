package maf.language.sexp

import maf.core._
import maf.language.scheme._

/** S-expressions and related values */
sealed trait Value
object Value:
    type SString = scala.Predef.String
    type SBoolean = scala.Boolean
    case class String(value: SString) extends Value:
        override def toString = s""""$value""""
    case class Symbol(sym: SString) extends Value:
        override def toString = s"'$sym"
    case class Integer(value: BigInt) extends Value:
        override def toString = value.toString
    case class Real(value: Double) extends Value:
        override def toString = f"$value%e".replace(",", ".").nn // Might not preserve full precision, but will be in a Scheme-compatible format
    case class Boolean(value: SBoolean) extends Value:
        override def toString = if value then "#t" else "#f"
    case class Character(value: Char) extends Value:
        override def toString = s"#\\$value"
    case object Nil extends Value:
        override def toString = "()"

/**
 * Abstract grammar elements for S-expressions include some positional information. This serves two purposes: identify where the s-expression resides
 * in the input file, and as tagging information for the abstract machine.
 */
sealed trait SExp extends Expression:
    val idn: Identity
    def fv: Set[String] = Set()

/**
 * An s-expression is made of pairs, e.g., (foo bar) is represented as the pair with identifier foo as car and another pair -- with identifier bar as
 * car and value nil as cdr -- as cdr. Pairs are pretty-printed when converted to string. i.e., (foo bar) is stringified as (foo bar) and not (foo .
 * (bar . ()))
 */
case class SExpPair(
    car: SExp,
    cdr: SExp,
    idn: Identity)
    extends SExp:
    override def toString: String =
        val content = toStringRest
        s"($content)"
    def toStringRest: String =
      cdr match
          case pair: SExpPair =>
            val rest = pair.toStringRest
            s"$car $rest"
          case SExpValue(Value.Nil, _) => s"$car"
          case _                       => s"$car . $cdr"
    val label: Label = Label.PAI
    def subexpressions: List[Expression] = List(car, cdr)

object SExpList:

    /** Alternative constructor to automatically construct a bunch of pair from a list of expressions */
    def apply(content: List[SExp], end: SExp): SExp = fromList(content, end)
    def apply(content: List[SExp], idn: Identity): SExp =
      fromList(content, SExpValue(Value.Nil, idn))

    def fromList(content: List[SExp], end: SExp): SExp = content match
        case Nil          => end
        case head :: tail => SExpPair(head, SExpList(tail, end), head.idn)

/** An identifier, such as foo, bar, etc. */
case class SExpId(id: Identifier) extends SExp:
    val idn: Identity = id.idn
    override def toString: String = id.toString
    val label: Label = Label.SYM
    def subexpressions: List[Expression] = List(id)

/** A literal value, such as 1, "foo", 'foo, etc. */
case class SExpValue(value: Value, idn: Identity) extends SExp:
    override def toString: String = value.toString
    val label: Label = Label.VAL
    def subexpressions: List[Expression] = List()
    override lazy val hash: Int = (label, value).hashCode()

/** A quoted element, such as 'foo, '(foo (bar)), etc. */
object SExpQuoted:
    def apply(content: SExp, idn: Identity): SExp =
      SExpList(List(SExpId(Identifier("quote", idn)), content), idn)

/** A quasiquoted element, such as `foo */
object SExpQuasiquoted:
    def apply(content: SExp, idn: Identity): SExp =
      SExpList(List(SExpId(Identifier("quasiquote", idn)), content), idn)

/** An unquoted element, such as ,foo */
object SExpUnquoted:
    def apply(content: SExp, idn: Identity): SExp =
      SExpList(List(SExpId(Identifier("unquote", idn)), content), idn)

/** A unquoted-splicing element, such as ,@foo */
object SExpUnquotedSplicing:
    def apply(content: SExp, idn: Identity): SExp =
      SExpList(List(SExpId(Identifier("unquote-splicing", idn)), content), idn)
