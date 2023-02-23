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
    def runAnalysis[A <: ModAnalysis[SchemeExp]](bench: String, analysis: String => A, worklist: String): A =
        val a = analysis(bench)
        print(s"Analysis of $bench with heuristic $worklist")
        try {
            val time = Timer.timeOnly {
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
        a

    val bench: List[String] = SchemeBenchmarkPrograms.fromFolder("test/R5RS/scp1")().toList

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


    bench.foreach({ b =>
        analyses.foreach((analysis, worklistName) =>
        runAnalysis(b, program => analysis(program),worklistName))
    })



