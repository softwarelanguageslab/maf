package maf.language.scheme.interpreter

import maf.language.sexp.{SExp, SExpParser}

import scala.util.parsing.input.CharSequenceReader

trait Handle:
    // The name used by this handle in the abstract interpreter. Used to convert values.
    val abstractName: String

object Handle:

    // !! These are *not* case classes because we want different handles if we open the same file multiple times

    class FileHandle(val filename: String) extends Handle:
        val abstractName: String = "__file__" + filename

        override def toString(): String = s"file:$filename"

    class StringHandle(val content: String) extends Handle:
        val abstractName: String = content

        override def toString(): String = s"string:$content"

    object ConsoleHandle extends Handle:
        val abstractName = "__console__"

        override def toString(): String = "console"

trait IO:
    def fromString(string: String): Handle

    def open(filename: String): Handle

    def close(f: Handle): Unit

    val console: Handle

    def read(f: Handle): Option[SExp]

    def readChar(f: Handle): ConcreteValues.Value

    def peekChar(f: Handle): ConcreteValues.Value

    def writeChar(c: Char, f: Handle): Unit

    def writeString(s: String, f: Handle): Unit

class EmptyIO extends IO:
    def fromString(string: String): Handle = new Handle.StringHandle(string)

    def open(filename: String): Handle = new Handle.FileHandle(filename)

    def close(h: Handle): Unit = ()

    val console = Handle.ConsoleHandle

    def read(h: Handle): Option[SExp] = None

    def readChar(h: Handle): ConcreteValues.Value = ConcreteValues.Value.EOF

    def peekChar(h: Handle): ConcreteValues.Value = ConcreteValues.Value.EOF

    def writeChar(c: Char, h: Handle): Unit = ()

    def writeString(s: String, h: Handle): Unit = ()

class FileIO(val files: Map[String, String]) extends IO:
    var positions: Map[Handle, Int] = Map.empty // Accesses to `positions` need synchronization on `this`.

    def fromString(string: String): Handle = synchronized {
      val handle = new Handle.StringHandle(string)
      positions = positions + (handle -> 0)
      handle
    }

    def open(filename: String): Handle = synchronized {
      if files.contains(filename) then
          val handle = new Handle.FileHandle(filename)
          positions = positions + (handle -> 0)
          handle
      else throw new Exception(s"Cannot open virtual file $filename")
    }

    def close(h: Handle): Unit = synchronized {
      h match
          case Handle.ConsoleHandle   => ()
          case h: Handle.FileHandle   => positions += h -> files(h.filename).size
          case h: Handle.StringHandle => positions += h -> h.content.size
    }

    val console = Handle.ConsoleHandle

    // Read: no need for synchronisation as long as files is immutable.
    def read(h: Handle): Option[SExp] = h match
        case Handle.ConsoleHandle   => None
        case h: Handle.FileHandle   => readUsingString(files(h.filename), h)
        case h: Handle.StringHandle => readUsingString(h.content, h)

    private def readUsingString(str: String, hdl: Handle): Option[SExp] = synchronized {
      val pos = positions(hdl)
      if pos >= str.size then None
      else
          val reader = new CharSequenceReader(str, pos)
          val (sexp, offset) = SExpParser.parseIn(reader)
          positions += hdl -> offset
          Some(sexp)
    }

    def readChar(h: Handle): ConcreteValues.Value = synchronized {
      peekChar(h) match
          case ConcreteValues.Value.EOF => ConcreteValues.Value.EOF
          case c =>
            positions = positions + (h -> (positions(h) + 1))
            c
    }

    def peekChar(h: Handle): ConcreteValues.Value = synchronized {
      h match
          case Handle.ConsoleHandle =>
            /* Console is never opened with this IO class */
            ConcreteValues.Value.EOF
          case h: Handle.FileHandle =>
            val pos = positions(h)
            if pos >= files(h.filename).size then ConcreteValues.Value.EOF
            else ConcreteValues.Value.Character(files(h.filename).charAt(pos))
          case h: Handle.StringHandle =>
            val pos = positions(h)
            if pos >= h.content.size then ConcreteValues.Value.EOF
            else ConcreteValues.Value.Character(h.content.charAt(pos))
    }

    def writeChar(c: Char, h: Handle): Unit = ()

    def writeString(s: String, h: Handle): Unit = ()
