package maf.syntax

import maf.util.SmartHash

/** The origin of a program text. */
enum Source:
    /** A program file, given by its path */
    case File(path: String)

    /** A definition from the prelude of the language (library code that is implicitly part of every program), given by its name */
    case Prelude(name: String)

    /** A program given directly as a string (e.g., in a test) */
    case Text

/**
 * A range in the source code of a program, from (`line`, `col`) up to, but not including, (`endLine`, `endCol`). Lines and columns are counted from
 * 1.
 *
 * Expressions carry an optional position (`Option[Position]`), which is `None` for expressions that do not occur in the source code (e.g., when they
 * are constructed by a program transformation).
 */
case class Position(source: Source, line: Int, col: Int, endLine: Int, endCol: Int) extends SmartHash:
    override def toString: String = source match
        case Source.File(path)    => s"$path:$line:$col"
        case Source.Prelude(name) => s"prelude:$name:$line:$col"
        case Source.Text          => s"$line:$col"

object Position:
    /** The position of an element read by the parser, from its start up to (but not including) its end */
    def apply(source: Source, start: scala.util.parsing.input.Position, end: scala.util.parsing.input.Position): Position =
        Position(source, start.line, start.column, end.line, end.column)

    extension (pos: Option[Position])
        /** Shows an optional position, using `?` for elements that do not occur in the source code */
        def show: String = pos.fold("?")(_.toString)
