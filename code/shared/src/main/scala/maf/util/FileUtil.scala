package maf.util

import maf.util.StringUtil.NumberedStrings

import java.io.*
import maf.util.Writer.Writer
import maf.util.benchmarks.Clock

import java.nio.file.{Paths, StandardCopyOption}

// null values are used here due to Java interop
import scala.language.unsafeNulls
import scala.collection.mutable

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

    private case class W(w: BufferedWriter, var report: Boolean)

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

object Formatter:

    //def toPercentString(value: Double, digits: Int = 2): String = f"${value*100}%.${digits}f%%"
    def toPercentString(
        num: Long,
        den: Long,
        digits: Int = 2
      ): String =
        val frac = num.toDouble / den.toDouble
        s"%.${digits}f".format(frac * 100) + "%"
    def withPercent(
        num: Long,
        den: Long,
        digits: Int = 2
      ) = s"$num (${toPercentString(num, den, digits)})"

/** Small utility to log messages in a structured way. */
object Logger:

    private val out: String = "logs/"

    class Log(private val writer: Writer):

        /** Indicates whether logging is enabled. */
        private var enabled: Boolean = true

        /** Re-enable logging. */
        def enable(): Unit = enabled = true

        /** (Temporarily disable logging) */
        def disable(): Unit = enabled = false

        /** Logs a message to a file. */
        def log(string: String): Unit = if enabled then
            Writer.write(writer, string + "\n")
            Writer.flush(writer)

        /** Logs a message to a file, and includes a timestamp. */
        def logT(string: String): Unit = if enabled then
            Writer.write(writer, s"${Clock.nowStr()} : $string\n")
            Writer.flush(writer)

        /** Logs an exception and its corresponding stacktrace to a file. */
        def logException(t: Throwable): Unit = if enabled then
            Writer.write(writer, t.toString + "\n" + t.getStackTrace.map(_.toString).mkString("\n"))
            Writer.flush(writer)

        def close(): Unit = Writer.close(writer)

    class NumberedLog(private val writer: Writer) extends Log(writer) with NumberedStrings:

        /** Logs a message to a file and adds a sequence number. */
        override def log(string: String): Unit = super.log(addSequenceNumber(string))

        /** Logs a message to a file without adding a sequence number. The message is not counted in the numbering. */
        def logU(string: String): Unit = super.log(string) // Log unnumbered.

        /** Logs a message to a file, and includes a timestamp and sequence number. */
        override def logT(string: String): Unit = super.logT(addSequenceNumber(string))

        /** Logs a message to a file, and includes a timestamp but no sequence number. The message is not counted in the numbering. */
        def logTU(string: String): Unit = super.logT(string) // Log unnumbered.

    def apply(msg: String = "log"): Log = new Log(Writer.openTimeStamped(out + msg + ".txt"))

    /** Opens a log without a timestamp */
    def raw(msg: String): Log = new Log(Writer.open(out + msg + ".txt"))

    def numbered(msg: String = "log"): NumberedLog = new NumberedLog(Writer.openTimeStamped(out + msg + ".txt"))

/** A global MAF Logger */
object MAFLogger:
    enum LogLevelPolicy:
        case Print
        case NoLog
        case FileLog(location: String)

    enum LogLevel:
        case AnalysisError
        case Info
        case Debug

    /** A series of environments that MAF can run in */
    enum LogEnvironment:
        case CI
        case Local
        case Benchmarking

    import LogLevelPolicy.*
    import LogLevel.*

    private def configs(env: LogEnvironment): Map[LogLevel, LogLevelPolicy] = env match
        case LogEnvironment.CI =>
            val logLocation: String = sys.env.get("LOG_LOCATION").getOrElse("log.txt")
            Map(
              AnalysisError -> NoLog, //FileLog(logLocation),
              Info -> NoLog,
              Debug -> NoLog
            )
        case LogEnvironment.Local =>
            Map(
              AnalysisError -> Print,
              Info -> Print,
              Debug -> Print
            )
        case LogEnvironment.Benchmarking =>
            Map(
              AnalysisError -> NoLog,
              Info -> NoLog,
              Debug -> NoLog
            )

    /** A mapping from log levels to actual loggers, depending on the policy */
    private val loggers: mutable.Map[LogLevel, Option[Logger.Log]] = mutable.Map(
      AnalysisError -> None,
      Info -> None,
      Debug -> None
    )

    /** Loads the config that is applicable for the current environment */
    private def currentConfig: Map[LogLevel, LogLevelPolicy] =
        sys.env.get("LOG_ENV").getOrElse("local") match
            case "local" => configs(LogEnvironment.Local)
            case "ci"    => configs(LogEnvironment.CI)
            case "bench" => configs(LogEnvironment.Benchmarking)
            case env     => throw new Exception(s"invalid logging environment $env")

    private var disabled: Boolean = false
    def enable(): Unit = disabled = false
    def disable(): Unit = disabled = true

    def log(level: LogLevel, msg: String): Unit =
        if disabled then return

        val policy = currentConfig(level)
        val finalMsg = s"[$level]$msg"
        policy match
            case Print       => println(finalMsg)
            case NoLog       => ()
            case FileLog(to) => () // disabled for the Javascript runtime for now
//val log = loggers(level)
//log match
//    case Some(log) => log.log(finalMsg)
//    case None =>
//        val logger = Logger.raw(to)
//        logger.enable()
//        loggers(level) = Some(logger)
//        logger.log(finalMsg)

object FileOps:

    def copy(source: String, destination: String): Unit =
        java.nio.file.Files.copy(Paths.get(source), Paths.get(destination), StandardCopyOption.REPLACE_EXISTING)

end FileOps
