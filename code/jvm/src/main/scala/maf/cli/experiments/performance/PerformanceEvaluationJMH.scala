package maf.cli.experiments.performance

import maf.language.scheme._
import maf.language.CScheme._
import maf.modular.ModAnalysis
import maf.util._
import maf.cli.experiments.SchemeAnalyses

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Setup}

trait PerformanceEvaluationJMH:
    def runProgram(file: String, analysis: SchemeExp => ModAnalysis[SchemeExp]): Unit =
        val program = CSchemeParser.parseProgram(Reader.loadFile(s"../../$file"))
        val _ = analysis(program).analyze()

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.SampleTime))
class PerformanceEvaluationJMHZeroCFA extends PerformanceEvaluationJMH:
    def analysis = SchemeAnalyses.kCFAAnalysis(_, 0)

    @Benchmark
    def mceval: Unit = runProgram("test/R5RS/various/mceval.scm", analysis)
    @Benchmark
    def leval: Unit = runProgram("test/R5RS/icp/icp_3_leval.scm", analysis)
    @Benchmark
    def sboyer: Unit = runProgram("test/R5RS/gambit/sboyer.scm", analysis)
    @Benchmark
    def scheme: Unit = runProgram("test/R5RS/gambit/scheme.scm", analysis)

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.SampleTime))
class PerformanceEvaluationJMHOneCFA extends PerformanceEvaluationJMH:
    def analysis = SchemeAnalyses.kCFAAnalysis(_, 1)

    @Benchmark
    def mceval: Unit = runProgram("test/R5RS/various/mceval.scm", analysis)
    @Benchmark
    def leval: Unit = runProgram("test/R5RS/icp/icp_3_leval.scm", analysis)
    @Benchmark
    def sboyer: Unit = runProgram("test/R5RS/gambit/sboyer.scm", analysis)
