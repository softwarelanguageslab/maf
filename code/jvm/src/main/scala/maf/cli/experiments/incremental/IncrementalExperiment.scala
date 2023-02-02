package maf.cli.experiments.incremental

import maf.core.Expression
import maf.modular.incremental.*
import maf.modular.incremental.IncrementalConfiguration.allConfigurations
import maf.util.ColouredFormatting.*
import maf.util.Writer.*
import maf.util.Writer
import maf.util.benchmarks.Timeout

import scala.concurrent.duration.{Duration, MINUTES}

trait IncrementalExperiment[E <: Expression]:

    // Type bound for an analysis.
    type Analysis <: IncrementalModAnalysis[E] with IncrementalGlobalStore[E]

    // Analysis construction.
    def analysis(e: E, config: IncrementalConfiguration): Analysis

    // The analysis configurations to use.
    var configurations: List[IncrementalConfiguration] = List()

    // Parsing.
    def parse(string: String): E

    // The timeout to be used. The timeout also indicates half of the time maximally spent on warm-up.
    def timeout(): Timeout.T = Timeout.start(Duration(minutes, MINUTES))
    var minutes: Int = 10

    // What is to be done for each benchmark program.
    def onBenchmark(file: String): Unit

    // Modifies the result to report an error during the experiments.
    def reportError(file: String): Unit

    // Where to write the results.
    val outputDir: String = "benchOutput/incremental/"
    val outputFile: String

    // Creates a string representation of the final output.
    def createOutput(): String

    // Can be used for debugging.
    var catchErrors: Boolean = true

    // Runs measurements on the benchmarks in a given trait, or uses specific benchmarks if passed as an argument.
    def measure(bench: Set[String]): Unit =
        val total = bench.size
        var count = 0
        bench.toList.sorted.foreach { file =>
            try
                count += 1
                println()
                println(markTask(s"Testing $file ($count of $total)"))
                onBenchmark(file)
            catch
                case e: Exception if catchErrors =>
                    // writeErrln(s"Running $file resulted in an exception: ${e.getMessage}")
                    //e.getStackTrace.nn.take(5).foreach(ste => writeErrln(ste.toString))
                    println(markError(s"Running $file resulted in an exception: ${e.getMessage}"))
                    reportError(file)
                case e: VirtualMachineError if catchErrors =>
                    // writeErrln(s"Running $file resulted in an error: ${e.getMessage}\n")
                    println(markError(s"Running $file resulted in an exception: ${e.getMessage}"))
                    reportError(file)
                case e => throw e
            println()
        }

    var output: Writer = _

    /** Ensure that an instance of an evaluation class can be used only once, to avoid polluting the wrong state. */
    private var executed = false

    /** Runs the benchmarks. Returns the path to the output file. */
    def execute(bench: Set[String], args: IncArgs): String =
        if executed then throw new Exception("Evaluation using this instance already executed. Create new instance of evaluation class.")
        if args.stopOnError then catchErrors = false
        if args.config.nonEmpty then configurations = List(args.config.get) else configurations = allConfigurations // Allows to override the default list of configurations of a setup.
        if args.timeout >= 0 then minutes = args.timeout
        executed = true
        val (writer, file): (Writer, String) = openTimeStampedGetName(outputDir + outputFile)
        output = writer
        Writer.enableReporting(output)
        measure(bench)
        val out: String = createOutput()
        writeln(output, out)
        Writer.close(output)
        file
