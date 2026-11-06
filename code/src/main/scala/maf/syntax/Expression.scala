package maf.syntax

import java.util.concurrent.atomic.AtomicLong

/**
 * An expression, i.e., a program point of a program.
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

object Expression:
    private val labels = new AtomicLong()
    private def freshLabel(): Long = labels.getAndIncrement()

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Option[Position]) extends Expression:
    def fullString: String = s"$name@${pos.show}"
    override def toString: String = name
