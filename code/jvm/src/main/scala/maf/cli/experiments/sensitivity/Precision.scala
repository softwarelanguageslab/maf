package maf.cli.experiments.sensitivity

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme._
import maf.lattice._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.util._
import maf.util.benchmarks.Timeout
import maf.cli.experiments.precision._
import maf.modular.worklist.LIFOWorklistAlgorithm

import scala.concurrent.duration._

object PrecisionComparison
    extends AnalysisComparison[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:
    val benchmarks =
      SchemeBenchmarkPrograms.gabriel ++
        List(
          "test/R5RS/mceval.scm"
        )

    var path: String = "<none>"

    // The base analysis
    def baseAnalysis(prg: SchemeExp): Analysis =
      new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeConstantPropagationDomain with LIFOWorklistAlgorithm[SchemeExp]

    // The analysis with improved precision
    def improvedAnalysis(prg: SchemeExp): Analysis =
      new SimpleSchemeModFAnalysis(prg)
        with SchemeModFUserGuidedSensitivity1
        with SchemeConstantPropagationDomain
        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "Improved"
      }

    def otherAnalyses() = List((improvedAnalysis, "Improved"))

    override def analysisTimeout() = Timeout.start(Duration(2, MINUTES)) // timeout for (non-base) analyses
    override def concreteTimeout() = Timeout.start(Duration(2, MINUTES))
    override def concreteRuns() = 1

    def main(args: Array[String]) = runBenchmarks() // check("test/primtest.scm")

    def check(benchmark: Benchmark) =
        val txt = Reader.loadFile(benchmark)
        val prg = SchemeParser.parseProgram(txt)
        val con = runInterpreter(prg, path).get
        val Terminated(abs) = runAnalysis(baseAnalysis, "base analysis", prg, path)
        val allKeys = con.keys ++ abs.keys
        allKeys.foreach { k =>
          println(s"$k -> ${abs.getOrElse(k, "⊥")} ; ${con.getOrElse(k, "⊥")} ")
        }

    def runBenchmarks() =
        benchmarks.foreach { b =>
            System.gc()
            path = b
            runBenchmark(b)
        }
        println("Results:")
        val columns = List("Improved", "concrete")
        println(results.withDefaultValue(None).prettyString(columns = columns, format = _.map(_.toString()).getOrElse("TIMEOUT")))
// println(results.withDefaultValue(None).toLatexString(columns = columns, format = _.map(_.toString()).getOrElse("--")))
