package maf.cli.experiments.precision

import maf.language.AScheme.ASchemeParser
import maf.modular.scheme.modactor.SchemeModActorSemantics
import maf.language.scheme.*
import maf.cli.experiments.SchemeAnalyses
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.Reader
import maf.language.AScheme.interpreter.ConcreteComponentsConversion
import maf.util.MonoidImplicits.FoldMapExtension

/**
 * Compare a number of concrete runs of the program with the output of the static analyser.
 *
 * We count the number of observed elements that are less precise than the concrete run, we call these elements spurious elements, because they do not
 * correspond with a concrete run of the program and are therefore imprecise.
 */
trait ModActorPrecisionBenchmarks extends ConcreteComponentsConversion:
    case class PrecisionResult()

    type Analysis <: SchemeModActorSemantics

    /**
     * Creates a static analysis for the given program
     *
     * @param program
     *   the program to analyse
     */
    def analysis(program: SchemeExp): Analysis

    /** The set of benchmark programs to use for the precision tests */
    def benchmarks: Set[String]

    /** The number of concrete runs to make before joining the runs together */
    def concreteRuns: Int = 10

    protected def alertMsg(msg: String): Unit = ()

    protected def parseProgram(source: String): SchemeExp =
        ASchemeParser.parseProgram(source)

    protected def onBenchmark(benchmark: String): PrecisionResult =
        val source = Reader.loadFile(benchmark)
        val program = parseProgram(source)
        // Run the concrete analysis a few times
        ???

    def run(outCsv: String): Unit = ???

object SimpleModActorPrecisionBenchmarks extends ModActorPrecisionBenchmarks:
    type Analysis = SimpleSchemeModActorAnalysis

    val benchmarks: Set[String] = SchemeBenchmarkPrograms.actors

    def analysis(program: SchemeExp) = SchemeAnalyses.modActorAnalysis(program)
    def main(args: Array[String]): Unit =
        run("benchOutput/precision/modactor-precision.csv")
