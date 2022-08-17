package maf.cli.runnables

import maf.modular.AnalysisEntry
import reflect.Selectable.reflectiveSelectable
import scala.io.StdIn.readLine
import maf.language.scheme.*
import concurrent.duration.*
import maf.cli.experiments.SchemeAnalyses
import maf.language.CScheme.CSchemeParser
import maf.util.Reader
import maf.util.benchmarks.Timeout

object Repl:
    val about: String = """
    | A simple configurable abstract interpreter for Scheme.
    |
    | Usage: sbt maf/runMain maf.cli.runnables.Repl -- [ARGUMENT ...]
    |
    | Arguments:
    | * -a ANALYSIS: change the type of analysis 
    | * -f FILENAME: the name of the Scheme file to analyze 
    | * -p PARSER: the parser to use 
    | * -i: interactive, means that a REPL-like structure will be spawned, but each line in the REPL is ran in an isolated environment
    """.stripMargin

    private val configurationsHelp: Map[String, String] = Map(
      "modfFS" -> "Flow sensitive ModF"
    )

    private val configurations: Map[String, (SchemeExp) => AnalysisEntry[SchemeExp]] = Map(
      "modfFS" -> SchemeAnalyses.modFFlowSensitive
    )

    private def printHelp(): Unit =
        println(about)
        println(" Supported analyses")
        configurationsHelp.foreach { case (configuration, help) =>
            println(s" * $configuration: $help")
        }

    case class ArgParser(
        remaining: List[String],
        analysis: Option[String] = None,
        filename: Option[String] = None,
        parser: Option[String] = None,
        interactive: Boolean = false):
        def isEmpty: Boolean = remaining.isEmpty
        def continue(remaining: List[String]): ArgParser = this.copy(remaining = remaining)
        def setAnalysis(analysis: String): ArgParser =
            ensureNotSet(this.analysis, "analysis")
            this.copy(analysis = Some(analysis))

        def setFilename(filename: String): ArgParser =
            ensureNotSet(this.filename, "filename")
            this.copy(filename = Some(filename))

        def setParser(parser: String): ArgParser =
            ensureNotSet(this.parser, "parser")
            this.copy(parser = Some(parser))

        def ensureNotSet[T](vlu: Option[T], field: String): Unit =
            if vlu.isDefined then throw new Exception(s"$field already set")

    object ArgParser:
        def init(remaining: List[String]): ArgParser = ArgParser(remaining)

    def parse(parser: ArgParser): ArgParser =
        if parser.isEmpty then parser
        else
            parser.remaining match
                case "-a" :: analysis :: rest =>
                    parse(parser.setAnalysis(analysis).continue(rest))

                case "-f" :: filename :: rest =>
                    parse(parser.setFilename(filename).continue(rest))

                case "-p" :: p :: rest =>
                    parse(parser.setParser(p).continue(rest))

                case "-i" :: rest =>
                    parse(parser.copy(interactive = true).continue(rest))

                case arg =>
                    throw new Exception(s"invalid arguments $arg")

    private type P = { def parse(e: String): SchemeExp }
    private type A = (SchemeExp) => AnalysisEntry[SchemeExp]

    private def setupParser(parser: Option[String]): P =
        // there is only one parser supported at the moment, so return that one
        new {
            def parse(e: String): SchemeExp =
                CSchemeParser.parseProgram(e)
        }

    private def setupAnalysis(analysis: String): (SchemeExp) => AnalysisEntry[SchemeExp] =
        configurations.get(analysis).getOrElse(throw new Exception(s"$analysis analysis not found"))

    /** Method to run the application in file-mode, which reads the file from disk and analyzes it using the configured analysis */
    private def runFile(filename: String, parser: P, makeAnalysis: A): Unit =
        val prog = Reader.loadFile(filename)
        val exp = parser.parse(prog)
        val anl = makeAnalysis(exp)
        anl.analyzeWithTimeout(Timeout.start(10.seconds))
        println(anl.result)

    /** Runs a REPL that can be used to interactively test the abstract interpreter */
    private def runRepl(parser: P, makeAnalysis: A): Unit =
        def repl(): Unit =
            print(">")
            val program = readLine().trim().nn
            if program != ":q" then
                val exp = parser.parse(program)
                val anl = makeAnalysis(exp)
                anl.analyzeWithTimeout(Timeout.start(10.seconds))
                println(anl.result)
                repl()
        repl()

    def main(args: Array[String]): Unit =
        if args.isEmpty then printHelp()
        else
            val listArgs = args.toList
            val options = parse(ArgParser.init(listArgs))
            // ensure that either "-f" or "-i" is set, but not both
            assert(!(options.filename.isDefined && options.interactive), "cannot use both -f and -i at the same time")
            assert(!(options.filename.isEmpty && !options.interactive), "provide either -f or -i")
            // ensure that the analysis is defined
            assert(options.analysis.isDefined, "define an analysis type using the -a argument")
            // setup the parser
            val parser = setupParser(options.parser)
            // setup the analysis
            val analysisFactory = setupAnalysis(options.analysis.get)
            // either run the file or the repl
            if options.interactive then runRepl(parser, analysisFactory) else runFile(options.filename.get, parser, analysisFactory)
