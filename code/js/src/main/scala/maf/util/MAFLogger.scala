package maf.util

/** A global MAF Logger */
object MAFLoggerWeb:
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

    def log(level: LogLevel, msg: String): Unit =
        val finalMsg = s"[$level]$msg"
        // no files to open in Scala.js
        println(finalMsg)
