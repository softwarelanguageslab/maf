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
import maf.modular.worklist.*
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeWorklistAlgorithms extends App :
  val analyses = List(
    (randomAnalysis, "RandomWorklistAlgorithm"))
  val bench: Map[String, String] = List(
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

  def runAnalysis[A <: ModAnalysis[SchemeExp]](bench: (String, SchemeExp), analysis: SchemeExp => A, worklist: String): (Map[String, Double], Long) =
    val a = analysis(bench._2)
    var time: Long = -1
    println(s"Analysis of ${bench._1} with heuristic $worklist")
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
    (a.timeMap, time)

  def randomAnalysis(program: SchemeExp) = new BasicAnalysis(program) with RandomWorklistAlgorithm[SchemeExp]

  def FIFOanalysis(program: SchemeExp) = new BasicAnalysis(program) with FIFOWorklistAlgorithm[SchemeExp]

  def LIFOanalysis(program: SchemeExp) = new BasicAnalysis(program) with LIFOWorklistAlgorithm[SchemeExp]

  def callDepthAnalysis(program: SchemeExp) = new BasicAnalysis(program) with CallDepthFirstWorklistAlgorithm[SchemeExp]

  def leastVisitedAnalysis(program: SchemeExp) = new BasicAnalysis(program) with LeastVisitedFirstWorklistAlgorithm[SchemeExp]

  def mostVisitedAnalysis(program: SchemeExp) = new BasicAnalysis(program) with MostVisitedFirstWorklistAlgorithm[SchemeExp]

  def deepExpressionFirstAnalysis(program: SchemeExp) = new BasicAnalysis(program) with DeepExpressionsFirstWorklistAlgorithm[SchemeExp]

  def shallowExpressionsFirstAnalysis(program: SchemeExp) = new BasicAnalysis(program) with ShallowExpressionsFirstWorklistAlgorithm[SchemeExp]

  def mostDependenciesFirstAnalysis(program: SchemeExp) = new BasicAnalysis(program) with MostDependenciesFirstWorklistAlgorithm[SchemeExp]

  def leastDependenciesFirstAnalysis(program: SchemeExp) = new BasicAnalysis(program) with LeastDependenciesFirstWorklistAlgorithm[SchemeExp]

  def biggerEnvironmentFirstAnalysis(program: SchemeExp) = new BasicAnalysis(program) with BiggerEnvironmentFirstWorklistAlgorithm.ModF

  def smallerEnvironmentFirstAnalysis(program: SchemeExp) = new BasicAnalysis(program) with SmallerEnvironmentFirstWorklistAlgorithm.ModF

  abstract class BasicAnalysis(program: SchemeExp) extends SimpleSchemeModFAnalysis(program)
    with SchemeConstantPropagationDomain
    with DependencyTracking[SchemeExp]
    with SchemeModFNoSensitivity {
    override def intraAnalysis(cmp: SchemeModFComponent) =
      new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
  }
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



