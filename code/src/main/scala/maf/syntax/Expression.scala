package maf.syntax

import java.util.concurrent.atomic.AtomicLong

/**
 * An expression, i.e., a node in the AST of a program.
 *
 * Every expression is a distinct program point: it is only equal to itself, even if another expression has the same structure and position (e.g.,
 * because a program transformation constructed both). To this end, every expression gets a unique label when it is constructed.
 */
trait Expression:

    /** The label of this expression, which is unique among all expressions (but differs between runs) */
    final val label: Long = Expression.freshLabel()

    final override def equals(that: Any): Boolean = that match
        case that: Expression => label == that.label
        case _                => false
    final override def hashCode: Int = java.lang.Long.hashCode(label)

    /** The position of the expression in its source file, or `None` if the expression does not occur in the source code. */
    def pos: Option[Position]

    /** The set of free variables appearing in this expression. */
    def fv: Set[String]

    /** The height of the AST represented by this expression. */
    val height: Int = -1

    /** Returns a list of direct subexpressions of the given expression. */
    def subexpressions: List[Expression] // Uses `def` instead of `val` to avoid continuous memory overheads.

    /** Returns a list of all subexpressions of an expression. */
    def allSubexpressions: List[Expression] =
        var subExpressions = this.subexpressions
        var toExplore = subExpressions // Start with the direct subexpressions.
        while toExplore.nonEmpty do
            var sub = toExplore.flatMap(_.subexpressions)
            subExpressions = sub ::: subExpressions
            toExplore = sub
        subExpressions

    def size: Int = allSubexpressions.size

object Expression:
    private val labels = new AtomicLong()
    private def freshLabel(): Long = labels.getAndIncrement()

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Option[Position]) extends Expression:
    def fullString: String = s"$name@${pos.show}"
    override def toString: String = name
    def fv: Set[String] = Set(name)
    override val height: Int = 0
    def subexpressions: List[Expression] = List()
