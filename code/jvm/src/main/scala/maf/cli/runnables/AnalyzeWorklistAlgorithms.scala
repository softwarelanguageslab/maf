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
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeWorklistAlgorithms extends App:
    def runAnalysis[A <: ModAnalysis[SchemeExp]](bench: (String, SchemeExp), analysis: SchemeExp => A, worklist: String): (Map[String, Double], Long) =
        val a = analysis(bench._2)
        var time: Long = -1
        //println(s"Analysis of $bench._1 with heuristic $worklist")
        try {
            time = Timer.timeOnly {
                a.analyze()
            }
            //println(s"terminated in ${time / 1000000} ms.")
        } catch {
            case t: Throwable =>
                println(s"raised exception.")
                System.err.println(t.getMessage)
                t.printStackTrace()
                System.err.flush()
        }
        (a.timeMap, time)

    /*def createAnalysis(program: SchemeExp)(worklistAlgorithm: WorklistAlgorithm[SchemeExp]): SimpleSchemeModFAnalysis = {
        new SimpleSchemeModFAnalysis(program)
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with SchemeModFKCallSiteSensitivity
          with worklistAlgorithm {
            val k = 2

            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }
    }

    def createAnalysis[A <: WorklistAlgorithm[SchemeExp]](program: SchemeExp)(worklistAlgorithm: A): SimpleSchemeModFAnalysis = {
        new SimpleSchemeModFAnalysis(program)
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with SchemeModFKCallSiteSensitivity
          with A {
            val k = 2

            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }
    }*/



    def randomAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with SchemeModFKCallSiteSensitivity
          with RandomWorklistAlgorithm[SchemeExp] {
            val k = 2
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def FIFOanalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with FIFOWorklistAlgorithm[SchemeExp] {
            val k = 2
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def LIFOanalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with LIFOWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def callDepthAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with CallDepthFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def leastVisitedAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with LeastVisitedFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def mostVisitedAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with MostVisitedFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def deepExpressionFirstAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with DeepExpressionsFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def shallowExpressionsFirstAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with ShallowExpressionsFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def mostDependenciesFirstAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with MostDependenciesFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def leastDependenciesFirstAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with LeastDependenciesFirstWorklistAlgorithm[SchemeExp] {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def biggerEnvironmentFirstAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with BiggerEnvironmentFirstWorklistAlgorithm.ModF {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def smallerEnvironmentFirstAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with SmallerEnvironmentFirstWorklistAlgorithm.ModF {
            val k = 0
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    val analyses = List(
        (randomAnalysis, "RandomWorklistAlgorithm"),
        (FIFOanalysis, "FIFOWorklistAlgorithm"),
        (LIFOanalysis, "LIFOWorklistAlgorithm"),
        (callDepthAnalysis, "CallDepthFirstWorklistAlgorithm"),
        (leastVisitedAnalysis, "LeastVisitedFirstWorklistAlgorithm"),
        (mostVisitedAnalysis, "MostVisitedFirstWorklistAlgorithm"),
        (deepExpressionFirstAnalysis, "DeepExpressionsFirstWorklistAlgorithm"),
        (shallowExpressionsFirstAnalysis, "ShallowExpressionsFirstWorklistAlgorithm"),
        (leastDependenciesFirstAnalysis, "MostDependenciesFirstWorklistAlgorithm"),
        (mostDependenciesFirstAnalysis, "LeastDependenciesFirstWorklistAlgorithm"),
        (biggerEnvironmentFirstAnalysis, "BiggerEnvironmentFirstWorklistAlgorithm"),
        (smallerEnvironmentFirstAnalysis, "SmallerEnvironmentFirstWorklistAlgorithm"))

    val bench: Map[String, String] = List(
        ("test/R5RS/scp1-compressed/all.scm", "scp"),
        ("test/R5RS/gambit/scheme.scm", "scheme"),
        ("test/R5RS/icp/icp_7_eceval.scm", "eceval"),
        ("test/R5RS/gambit/sboyer.scm", "sboyer"),
        ("test/R5RS/gambit/peval.scm", "peval"),
        ("test/R5RS/icp/icp_1c_multiple-dwelling.scm", "multiple-dwelling"),
        ("test/R5RS/icp/icp_1c_ontleed.scm", "decompose"),
        ("test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm", "dynamic"),
        ("test/R5RS/icp/icp_1c_prime-sum-pair.scm", "prime-sum-pair"),
        ("test/R5RS/icp/icp_1c_ambeval.scm", "ambeval"),
        ("test/R5RS/WeiChenRompf2019/meta-circ.scm", "meta-circ"),
        ("test/R5RS/WeiChenRompf2019/toplas98/boyer.scm", "boyer"),
        ("test/R5RS/gambit/nboyer.scm", "nboyer"),
        ("test/R5RS/various/SICP-compiler.scm", "SICP-compiler"),
        ("test/R5RS/icp/icp_8_compiler.scm", "compiler"),
        ("test/R5RS/ad/all.scm", "ad"),
        ("test/R5RS/icp/icp_3_leval.scm", "leval"),
        ("test/R5RS/icp/icp_2_aeval.scm", "aeval"),
        ("test/R5RS/WeiChenRompf2019/earley.sch", "earley"),
        ("test/R5RS/WeiChenRompf2019/toplas98/graphs.scm", "graphs"),
        ("test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm", "nbody"),
        ("test/R5RS/gambit/matrix.scm", "matrix"),
        ("test/R5RS/gambit/browse.scm", "browse"),
        ("test/R5RS/icp/icp_5_regsim.scm", "regsim"),
    ).toMap



    val warmup = 3
    val numIterations = 10
    bench.foreach({ b =>
        val program = SchemeParser.parseProgram(Reader.loadFile(b._1)) // doing parsing only once
        analyses.foreach((analysis, worklistName) => {
            val results = (1 to (warmup + numIterations)).map(_ =>
                val result = runAnalysis((b._2, program), program => analysis(program), worklistName)
                result._2
            )
            val avgTime = results.drop(warmup).sum / numIterations.toDouble
            println(s"Average time for $worklistName on ${b._2}: ${avgTime / 1000000.0} ms.")
            println()
            println()
        })
    })



