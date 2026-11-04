package maf.core

import maf.core.Identity._
import maf.core.Position._
import maf.util.SmartHash

/** An identity to distinguish expressions. */
sealed trait Identity extends Serializable:
    val idn: IDN
    def pos: Position = idn // Extra positional information of the element in the source code. Used for printing and during tests.
    override def toString: String = pos.toString

/** An identity for AST elements. */
case class SimpleIdentity(idn: IDN) extends Identity with SmartHash

/** Neutral identity for to elements not in the code (constructed by the analysis). */
case object NoCodeIdentity extends Identity:
    val idn: IDN = Identity.newId(Position(-1, 0))
trait PositionalIdentity:
    type IDN = Position
    def apply(p: scala.util.parsing.input.Position, t: PTag = noTag): Identity = SimpleIdentity(Position(p.line, p.column, t))
    def newId(pos: Position): IDN = pos

object Identity extends PositionalIdentity:
    def none: Identity = NoCodeIdentity

object Position:
    // Tag for positions (can e.g. be used when ASTs of multiple parsings need to be combined).
    sealed trait PTag:
        /** Geneates a showable representation of the tag */
        def show: String

        /** Checks wheter the tag contains the given string */
        def contains(v: String): Boolean

        /** Returns true if the position does not have a tag */
        def isEmpty: Boolean = false

    case class SimplePTag(tag: String) extends PTag:
        def show: String = tag
        def contains(v: String): Boolean = tag.contains(v)

    case class PTagWithSource(tag: String, path: String) extends PTag:
        def show: String = s"$tag:$path"
        def contains(v: String): Boolean = tag.contains(v)

    case class SourcePathTag(sourcePath: String) extends PTag:
        def show: String = s"$sourcePath"
        def contains(v: String): Boolean = false
        override def isEmpty: Boolean = true

    case object NoPTag extends PTag:
        override def isEmpty: Boolean = true
        def show: String = ""
        def contains(v: String): Boolean = false

    val noTag: PTag = NoPTag
    def newTag(tag: String): PTag = SimplePTag(tag)
    def newTag(tag: String, path: String): PTag = PTagWithSource(tag, path)
    def withSourcePath(path: String) = SourcePathTag(path)

    sealed trait Position extends SmartHash:
        def line: Int
        def col: Int
        def tag: PTag

    case class TextualPosition(line: Int, col: Int, tag: PTag = noTag) extends Position:
        override def toString: String = tag match
            case NoPTag => s"$line:$col"
            case t      => s"${t.show}:$line:$col"

    /**
     * Positional information of an expression.
     *
     * @param line
     *   the line on which the expression occurs in the source code
     * @param col
     *   the column on which the expression occurs in the source code
     * @param tag
     *   an optional tag, can be used to differentiate the AST from multiple parsings
     */
    object Position:
        def apply(
            line: Int,
            col: Int,
            tag: PTag = noTag
          ): Position = TextualPosition(line, col, tag)

    def apply(
        line: Int,
        col: Int,
        tag: PTag = noTag
      ): Position = Position(line, col, tag)
