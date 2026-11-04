package maf.util

import java.io.*
import maf.util.Writer.Writer
import maf.util.benchmarks.Clock

// null values are used here due to Java interop
import scala.language.unsafeNulls

object Reader:

    def loadFile(file: String): String =
        val fHandle = scala.io.Source.fromFile(file)
        val content = fHandle.getLines().mkString("\n")
        fHandle.close()
        content

/**
 * Utility to write to plain text files. Can also be modified to write to csv files using a CSVWriter, CSVWriter.NO_QUOTE_CHARACTER and the writeNext
 * method.
 */
object Writer:

    // Todo: W used to be a private class, but this started to give IllegalAccessErrors at some point. Try to make this private again?
    case class W(w: BufferedWriter, var report: Boolean)
    opaque type Writer = W

    /** Avoids a file to be accidentally overwritten. */
    private def addSuffix(f: File): File =
        var file = f
        if !file.exists() then return file
        val path = file.getPath
        val idx = path.lastIndexOf(".")
        val (name, ext) = (path.substring(0, idx), path.substring(idx))
        var suffix = 1
        while file.exists() do
            val n = s"$name-$suffix$ext"
            file = new File(n)
            suffix += 1
        file

    def open(path: String, avoidDuplicate: Boolean = false): Writer =
        val file = new File(path)
        file.getParentFile().nn.mkdirs() // Creates the directory containing the file if it does not exists.
        W(new BufferedWriter(new FileWriter(if avoidDuplicate then addSuffix(file) else file)), false)

    def openTimeStamped(path: String): Writer =
        path.split("\\.").nn match
            case Array(file, ext) => open(file + " " + Clock.nowStr() + "." + ext, true)
            case _                => throw new Exception(s"Illegal path: $path")

    def openTimeStampedGetName(path: String): (Writer, String) = // Also returns the name of the file.
        path.split("\\.").nn match
            case Array(file, ext) =>
                val out = file + " " + Clock.nowStr() + "." + ext
                (open(out), out)
            case _ => throw new Exception(s"Illegal path: $path")

    def close(writer: Writer): Unit = writer._1.close()

    def enableReporting(writer: Writer): Unit = writer.report = true
    def disableReporting(writer: Writer): Unit = writer.report = false

    // Avoid output being buffered.
    def write(writer: Writer, data: String): String =
        writer.w.write(data)
        writer.w.flush()
        if writer.report then
            System.out.print(data)
            System.out.flush()
        data
    def writeln(writer: Writer, data: String): String = write(writer, data + "\n")

    def writeErr(writer: Writer, data: String): String =
        System.err.print(data)
        System.err.flush()
        writer.w.write(data)
        writer.w.flush()
        data
    def writeErrln(writer: Writer, data: String): String = writeErr(writer, data + "\n")

    def flush(writer: Writer): Unit = writer._1.flush()

