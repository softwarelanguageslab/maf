package maf.syntax.sexp

import maf.syntax.*

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
    val pos: Option[Position]

/**
 * An s-expression is made of pairs, e.g., (foo bar) is represented as the pair with identifier foo as car and another pair -- with identifier bar as
 * car and value nil as cdr -- as cdr. Pairs are pretty-printed when converted to string. i.e., (foo bar) is stringified as (foo bar) and not (foo .
 * (bar . ()))
 */
case class SExpPair(
    car: SExp,
    cdr: SExp,
    pos: Option[Position])
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

case class SExpVector(
    elements: List[SExp],
    pos: Option[Position])
    extends SExp:

    override def toString: String = s"#(${elements.map(_.toString).mkString(" ")})"

object SExpList:

    /** Constructs the pairs of a proper list with the given elements, where the entire list has position `pos` */
    def apply(content: List[SExp], pos: Option[Position]): SExp =
        // the empty list at the end spans from the last element up to the end of the list (i.e., its closing parenthesis)
        apply(content, SExpValue(Value.Nil, content.lastOption.fold(pos)(after(_, pos))), pos)

    /**
     * Constructs the pairs of a list with the given elements that ends in `end` (e.g., a dotted list), where the entire list has position `pos`. The
     * pair of each element spans from that element up to the end of the list.
     */
    def apply(content: List[SExp], end: SExp, pos: Option[Position]): SExp = content match
        case Nil          => end
        case head :: tail => SExpPair(head, SExpList(tail, end, tail.headOption.fold(pos)(from(_, pos))), pos)

    /** The part of `pos` that starts where `e` starts */
    private def from(e: SExp, pos: Option[Position]): Option[Position] =
        for p <- pos; s <- e.pos yield p.copy(line = s.line, col = s.col)

    /** The part of `pos` that starts where `e` ends */
    private def after(e: SExp, pos: Option[Position]): Option[Position] =
        for p <- pos; s <- e.pos yield p.copy(line = s.endLine, col = s.endCol)

/** An identifier, such as foo, bar, etc. */
case class SExpId(id: Identifier) extends SExp:
    val pos: Option[Position] = id.pos
    override def toString: String = id.toString

/** A literal value, such as 1, "foo", 'foo, etc. */
case class SExpValue(value: Value, pos: Option[Position]) extends SExp:
    override def toString: String = value.toString

/** A quoted element, such as 'foo, '(foo (bar)), etc. */
object SExpQuoted:
    def apply(content: SExp, quotePos: Option[Position], pos: Option[Position]): SExp =
        SExpList(List(SExpId(Identifier("quote", quotePos)), content), pos)

/** A quasiquoted element, such as `foo */
object SExpQuasiquoted:
    def apply(content: SExp, quotePos: Option[Position], pos: Option[Position]): SExp =
        SExpList(List(SExpId(Identifier("quasiquote", quotePos)), content), pos)

/** An unquoted element, such as ,foo */
object SExpUnquoted:
    def apply(content: SExp, quotePos: Option[Position], pos: Option[Position]): SExp =
        SExpList(List(SExpId(Identifier("unquote", quotePos)), content), pos)

/** A unquoted-splicing element, such as ,@foo */
object SExpUnquotedSplicing:
    def apply(content: SExp, quotePos: Option[Position], pos: Option[Position]): SExp =
        SExpList(List(SExpId(Identifier("unquote-splicing", quotePos)), content), pos)
