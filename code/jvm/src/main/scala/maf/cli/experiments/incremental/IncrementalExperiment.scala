package maf.cli.experiments.incremental

import maf.core.Expression
import maf.modular.incremental.*
import maf.util.Writer.*
import maf.util.Writer
import maf.util.benchmarks.Timeout

trait IncrementalExperiment[E <: Expression]:

    // Type bound for an analysis.
    type Analysis <: IncrementalModAnalysis[E] with IncrementalGlobalStore[E]

    // Analysis construction.
    def analysis(e: E, config: IncrementalConfiguration): Analysis

    // The analysis configurations to use.
    val configurations: List[IncrementalConfiguration]

    // Parsing.
    def parse(string: String): E

    // The timeout to be used. The timeout also indicates half of the time maximally spent on warm-up.
    def timeout(): Timeout.T

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
    val catchErrors: Boolean = true

    // Runs measurements on the benchmarks in a given trait, or uses specific benchmarks if passed as an argument.
    def measure(bench: Set[String]): Unit =
        val total = bench.size
        var count = 0
        bench.toList.sorted.foreach { file =>
            try
                count += 1
                println(s"\nTesting $file ($count of $total)")
                onBenchmark(file)
            catch
                case e: Exception if catchErrors =>
                    // writeErrln(s"Running $file resulted in an exception: ${e.getMessage}")
                    //e.getStackTrace.nn.take(5).foreach(ste => writeErrln(ste.toString))
                    reportError(file)
                case e: VirtualMachineError =>
                    // writeErrln(s"Running $file resulted in an error: ${e.getMessage}\n")
                    reportError(file)
            println()
        }

    var output: Writer = _

    /** Ensure that an instance of an evaluation class can be used only once, to avoid polluting the wrong state. */
    private var executed = false

    /** Runs the benchmarks. Returns the path to the output file. */
    def execute(args: Set[String]): String =
        if executed then throw new Exception("Evaluation using this instance already executed. Create new instance of evaluation class.")
        executed = true
        val (writer, file): (Writer, String) = openTimeStampedGetName(outputDir + outputFile)
        output = writer
        Writer.enableReporting(output)
        measure(args)
        val out: String = createOutput()
        writeln(output, out)
        Writer.close(output)
        file
