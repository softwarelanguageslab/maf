package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.SchemeAnalyses
import maf.core.{Identifier, Monad}
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.ProgramVersionExtracter.*
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.worklist.{FIFOWorklistAlgorithm, *}
import maf.util.Reader
import maf.util.benchmarks.Timer

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeWorklistAlgorithms extends App:
    def runAnalysis[A <: ModAnalysis[SchemeExp]](bench: String, analysis: String => A, worklist: String): Long =
        val a = analysis(bench)
        var time: Long = -1
        println(s"Analysis of $bench with heuristic $worklist")
        try {
            time = Timer.timeOnly {
                a.analyze()
            }
            println(s"terminated in ${time / 1000000} ms.")
        } catch {
            case t: Throwable =>
                println(s"raised exception.")
                System.err.println(t.getMessage)
                t.printStackTrace()
                System.err.flush()
        }
        time

    val bench: List[String] = SchemeBenchmarkPrograms.fromFolder("test/R5RS/icp")().toList

    def FIFOanalysis(text: String) =
        val program = SchemeParser.parseProgram(text)
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFNoSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def LIFOanalysis(text: String) =
        val program = SchemeParser.parseProgram(text)
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFNoSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with LIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    val analyses = List((FIFOanalysis, "FIFOWorklistAlgorithm"), (LIFOanalysis, "LIFOWorklistAlgorithm"))
    val warmup = 3
    val numIterations = 10

    bench.foreach({ b =>
        analyses.foreach((analysis, worklistName) => {
            val results = (1 to (warmup + numIterations)).map(_ =>
                runAnalysis(b, program => analysis(program), worklistName)
            )
            val avgTime = results.sum / numIterations
            println(s"Average time for $worklistName on $b: ${avgTime / 1000000} ms.")
            println()
            println()
        })
    })



