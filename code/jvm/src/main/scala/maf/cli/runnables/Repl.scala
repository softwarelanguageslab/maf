package maf.cli.runnables

import maf.language.racket.*
import maf.modular.AnalysisEntry
import reflect.Selectable.reflectiveSelectable
import scala.io.StdIn.readLine
import maf.language.scheme.*
import concurrent.duration.*
import maf.cli.experiments.SchemeAnalyses
import maf.util.Reader
import maf.util.benchmarks.Timeout
import maf.util.benchmarks.Timer
import maf.util.benchmarks.Statistics
import maf.cli.experiments.aam.AAMAnalyses
import maf.modular.scheme.monadic.SimpleModFAnalysis
import maf.cli.experiments.precision.{SingleBenchmark}

object Repl:
    val about: String = """
    | A simple configurable abstract interpreter for Scheme.
    |
    | Usage: sbt maf/runMain maf.cli.runnables.Repl -- [ARGUMENT ...]
    |
    | Arguments:
    | * -a ANALYSIS: change the type of analysis 
    | * -kcfa CALLSITE SENSITIVITY: use a k-call-site sensitive analysis. Cannot be used at the same time as "-a"
    | * -f FILENAME: the name of the Scheme file to analyze 
    | * -p PARSER: the parser to use 
    | * -i interactive, means that a REPL-like structure will be spawned, but each line in the REPL is ran in an isolated environment
    | * -perf run the analysis in performance measuring mode, only works with "-f".
    | * -t TIMEOUT: run the analysis with the given timeout (in seconds). Defaults to 10.
    | * -dot  set flag to enable outputting a FILENAME.dot file that contains a visualisation of results of the analysis, only works with "-f"
    | * -m load the given Racket module, use instead of "-f"
    """.stripMargin

    private val configurationsHelp: Map[String, String] = Map(
      "modfFS" -> "Flow sensitive ModF",
      "modf" -> "Normal ModF Analysis",
      "aam" -> "AAM-style analysis",
      "ci" -> "context-insensitive analysis",
      "modfMonad" -> "default modf analysis with state as case class"
    )

    private val parserHelp: Map[String, String] = Map(
      "default" -> "The default Scheme parser."
    )

    private val configurations: Map[String, (SchemeExp) => AnalysisEntry[SchemeExp]] = Map(
      "aam" -> AAMAnalyses.aamBase,
      "modf" -> SchemeAnalyses.contextInsensitiveAnalysis,
      "modfFS" -> SchemeAnalyses.modFFlowSensitive,
      "ci" -> SchemeAnalyses.contextInsensitiveAnalysis,
      "modfMonad" -> ((exp) => new SimpleModFAnalysis(exp))
    )

    private def printHelp(): Unit =
        println(about)
        println(" Supported analyses")
        configurationsHelp.foreach { case (configuration, help) =>
            println(s" * $configuration: $help")
        }
        println(" Supported parsers")
        parserHelp.foreach { case (parser, help) =>
            println(s" * $parser: $help")
        }

    case class ArgParser(
        remaining: List[String],
        analysis: Option[String] = None,
        filename: Option[String] = None,
        parser: Option[String] = None,
        module: Option[String] = None,
        kcfa: Option[Int] = None,
        interactive: Boolean = false,
        performance: Boolean = false,
        precision: Boolean = false,
        dot: Boolean = false,
        timeout: Long = 10):
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

        def setModule(moduleName: String): ArgParser =
            ensureNotSet(this.module, "module")
            this.copy(module = Some(moduleName))

        def setKcfa(k: String): ArgParser =
            ensureNotSet(this.kcfa, "kcfa")
            this.copy(kcfa = Some(k.toInt))

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

                case "-m" :: filename :: rest =>
                    parse(parser.setModule(filename).continue(rest))

                case "-p" :: p :: rest =>
                    parse(parser.setParser(p).continue(rest))

                case "-i" :: rest =>
                    parse(parser.copy(interactive = true).continue(rest))

                case "-perf" :: rest =>
                    parse(parser.copy(performance = true).continue(rest))

                case "-prec" :: rest =>
                    parse(parser.copy(precision = true).continue(rest))

                case "-t" :: timeout :: rest =>
                    parse(parser.copy(timeout = timeout.toLong).continue(rest))

                case "-dot" :: rest =>
                    parse(parser.copy(dot = true).continue(rest))

                case "-kcfa" :: k :: rest => 
                    parse(parser.setKcfa(k).continue(rest))

                case arg =>
                    throw new Exception(s"invalid arguments $arg")

    private type P = { def parse(e: String): SchemeExp; def parseDefines(e: String): SchemeExp }
    private type A = (SchemeExp) => AnalysisEntry[SchemeExp]

    private def setupParser(parser: Option[String]): P = parser match
        case _ =>
            // there is only one parser supported at the moment, so return that one
            new {
                def parse(e: String): SchemeExp =
                    SchemeParser.parseProgram(e)
                def parseDefines(e: String): SchemeExp =
                    // SchemeParser.parseProgramDefines(e)
                    SchemeParser.parseProgram(e)
            }

    private type Path = String
    private def setupLoader(parser: P, enableModuleLoader: Boolean): Path => SchemeExp =
        if enableModuleLoader then Modules.path andThen GenericRacketLoader(parser.parseDefines).load
        else parser.parse

    private def setupAnalysis(kcfa: Option[Int], analysis: Option[String]): (SchemeExp) => AnalysisEntry[SchemeExp] =
        kcfa match {
            case Some(k) => SchemeAnalyses.kCFAAnalysis(_, k)
            case None    => configurations.get(analysis.get).getOrElse(throw new Exception(s"$analysis analysis not found"))
        }

    /** Method to run the application in file-mode, which reads the file from disk and analyzes it using the configured analysis */
    private def runFile(
        filename: String,
        parser: P,
        makeAnalysis: A,
        performance: Boolean,
        timeout: Long,
        dot: Boolean,
        someLoader: Option[String => SchemeExp] = None,
        precision: Boolean
      ): Unit =
        val loader: String => SchemeExp = someLoader.getOrElse(Reader.loadFile andThen parser.parse)
        // Regardless of the performance mode, we parse the file only once.
        val exp = loader(filename)
        def runSingle(): Long =
            val anl = makeAnalysis(exp)
            val (elapsed, _) = Timer.time { anl.analyzeWithTimeout(Timeout.start(timeout.seconds)) }
            // Do not print results if we are in perfomance testing mode
            if !performance then
                if !anl.finished then println("Analysis timed out")
                anl.printResult
                println(s"Analysis took ${elapsed / (1000 * 1000)} ms")
            // Print a dot graph if the dot option has been enabled
            if dot then anl.toDot(filename.replace("/", "_").nn + ".dot")
            elapsed
        
        def runPrecision(): Unit = 
            val anl = makeAnalysis(exp)
            val benchmarkRunner = new SingleBenchmark(anl, filename)
            benchmarkRunner.main(Array.empty[String])

        val warmUpTimes = 5
        val analysisTimes = 10

        if performance then
            print("Warm up ")
            // ignore the results of the warm up phase
            (0 until warmUpTimes).foreach(i =>
                print(s"$i ")
                runSingle()
            )
            println()

            print("Analysis ")
            // take the average of the elapsed times
            val elapseds = (0 until analysisTimes)
                .map(i =>
                    print(s"$i ")
                    // try GC between runs
                    System.gc()
                    // run the benchmark
                    runSingle() / (1000 * 1000)
                )
                .map(_.toDouble)
                .toList
            println()
            // compute the results
            val elapsedMean = Statistics.mean(elapseds)
            val stddev = Statistics.stddev(elapseds)
            println(s"Average analysis time: $elapsedMean ms")
            println(s"Standard devitation: $stddev ms")
        else if precision then 
            runPrecision()
        else runSingle()

    /** Runs a REPL that can be used to interactively test the abstract interpreter */
    private def runRepl(parser: P, makeAnalysis: A): Unit =
        def repl(): Unit =
            print(">")
            val program = readLine().trim().nn
            if program != ":q" then
                val exp = parser.parse(program)
                val anl = makeAnalysis(exp)
                val (elapsed, _) = Timer.time { anl.analyzeWithTimeout(Timeout.start(10.seconds)) }

                anl.printResult
                println(s"Analysis took ${elapsed / (1000 * 1000)} ms")
                repl()
        repl()

    def main(args: Array[String]): Unit =
        if args.isEmpty then printHelp()
        else
            val listArgs = args.toList
            val options = parse(ArgParser.init(listArgs))
            // ensure that either "-f" or "-i" is set, but not both
            assert(!((options.filename.isDefined || options.module.isDefined) && options.interactive), "cannot use both -f and -i at the same time")
            assert(!(options.filename.isEmpty && options.module.isEmpty && !options.interactive), "provide either -f or -i")
            assert(!(options.kcfa.isEmpty && options.analysis.isEmpty && !options.interactive), "provide either -kcfa or -a")
            // ensure that -perf is only used in combination with -f
            assert(if options.performance then options.filename.isDefined else true, "performance measuring mode must be used in file mode")
            // ensure that "-dot" is combined with "-f"
            assert(if options.dot then options.filename.isDefined else true, "-dot can only be combined with -f")
            // ensure that "-m" is not combined with "-f"
            assert(if options.module.isDefined then !options.filename.isDefined else true, "-m can not be combined with -f")
            // setup the parser
            val parser = setupParser(options.parser)
            // setup the loader
            val loader =
                if options.module.isDefined then Some(setupLoader(parser, options.module.isDefined))
                else None
            // setup the analysis
            val analysisFactory = setupAnalysis(options.kcfa, options.analysis)
            // setup the loader of the file/module
            // either run the file or the repl
            if options.interactive then runRepl(parser, analysisFactory)
            else
                // retrieve the file or module name
                val path = options.filename.getOrElse(options.module.get)
                runFile(path, parser, analysisFactory, options.performance, options.timeout, options.dot, loader, options.precision)
