package maf.util

import java.io._

import maf.util.Writer.Writer
import maf.util.benchmarks.Clock

object Reader {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines().mkString("\n")
    fHandle.close()
    content
  }

}

/**
 * Utility to write to plain text files. Can also be modifief to write to csv files using
 * a CSVWriter, CSVWriter.NO_QUOTE_CHARACTER and the writeNext method.
 */
object Writer {

  type Writer = BufferedWriter

  private var defaultWriter: Writer = _
  var report: Boolean = false

  def open(path: String): Writer =
    new BufferedWriter(new FileWriter(path))

  def openTimeStamped(path: String): Writer =
    path.split("\\.") match {
      case Array(file, ext) => open(file + "_" + Clock.nowStr() + "." + ext)
      case _                => throw new Exception(s"Illegal path: $path")
    }

  def close(writer: Writer): Unit = writer.close()

  def setDefaultWriter(writer: Writer): Unit = defaultWriter = writer
  def closeDefaultWriter(): Unit = defaultWriter.close()

  def enableReporting(): Unit = report = true
  def disableReporting(): Unit = report = false

  // Avoid output being buffered.
  def write(writer: Writer, data: String): String = {
    writer.write(data)
    writer.flush()
    if (report) {
      System.out.print(data)
      System.out.flush()
    }
    data
  }

  def writeln(writer: Writer, data: String): String = write(writer, data + "\n")

  def write(data: String = "\n"): String = write(defaultWriter, data)
  def writeln(data: String = "\n"): String = writeln(defaultWriter, data)

  def writeErr(writer: Writer, data: String): String = {
    System.err.print(data)
    System.err.flush()
    writer.write(data)
    writer.flush()
    data
  }

  def writeErrln(writer: Writer, data: String): String = writeErr(writer, data + "\n")

  def writeErr(data: String = "\n"): String = writeErr(defaultWriter, data)
  def writeErrln(data: String = "\n"): String = writeErrln(defaultWriter, data)

}

object Formatter {

  //def toPercentString(value: Double, digits: Int = 2): String = f"${value*100}%.${digits}f%%"
  def toPercentString(
      num: Long,
      den: Long,
      digits: Int = 2
    ): String = {
    val frac = num.toDouble / den.toDouble
    s"%.${digits}f".format(frac * 100) + "%"
  }
  def withPercent(
      num: Long,
      den: Long,
      digits: Int = 2
    ) = s"$num (${toPercentString(num, den, digits)})"
}

/** Small utility to log messages in a structured way. */
object Logger {

  private val out: String = "logs/"

  class Log(private val writer: Writer) {
    def log(string: String): Unit = {
      writer.write(string + "\n")
      writer.flush()
    }

    def logT(string: String): Unit = {
      writer.write(s"${Clock.nowStr()} : $string\n")
      writer.flush()
    }

    def close(): Unit = writer.close()
  }

  class NumberedLog(private val writer: Writer) extends Log(writer) {

    private var count: Int = 1

    /** Adds a line number to the given string. */
    private def addCount(s: String): String = {
      count += 1
      s"${toWidth(count.toString + ".", 5)} $s"
    }

    /** Ensures a string has the given width by filling with whitespaces. If the string is longer than the given width, the string will be returned. */
    private def toWidth(s: String, w: Int): String = s + (" " * (w - s.length))

    override def log(string: String): Unit = super.log(addCount(string))

    def logU(string: String) = super.log(string) // Log unnumbered.

    override def logT(string: String): Unit = super.logT(addCount(string))

    def logTU(string: String): Unit = super.logT(string) // Log unnumbered.

    def resetNumbering(): Unit = count = 1
  }

  def apply(msg: String = "log"): Log = new Log(Writer.openTimeStamped(out + msg + ".txt"))

  def numbered(msg: String = "log"): NumberedLog = new NumberedLog(Writer.openTimeStamped(out + msg + ".txt"))
}
