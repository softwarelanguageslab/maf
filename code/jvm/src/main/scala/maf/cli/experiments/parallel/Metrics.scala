package maf.cli.experiments.parallel

import maf.core._
import maf.language.scheme._
import maf.language.CScheme._
import maf.util._
import maf.util.benchmarks._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._

trait Metric {
  type M
  def name: String
  def forProgram(program: SchemeExp): M
}

object Metric {
  case class SequenceBasedMetric(vs: List[Int]) {
    def mean = Statistics.mean(vs.map(_.toDouble)).toInt
    def median = Statistics.median(vs.map(_.toDouble)).toInt
    def stddev = Statistics.stddev(vs.map(_.toDouble)).toInt
    def max = vs.max.toInt
    def add(v: Int): SequenceBasedMetric = SequenceBasedMetric(v :: vs)
    override def toString = s"$mean [$median±$stddev] <= $max"
  }

  class CallDepth(val kCFA: Int) extends Metric {
    def name = s"call-depth($kCFA)"
    type M = SequenceBasedMetric

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
          with SchemeModFSemantics
          with StandardSchemeModFComponents
          with BigStepModFSemantics
          with CallDepthFirstWorklistAlgorithm[SchemeExp]
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.depth.values.toList)
    }
  }

  class Visited(kCFA: Int) extends Metric {
    // TODO: two concerns for this metric:
    //   - Can the number of visits may change between different analyses of the same file? If everything is deterministic, I guess not
    //   - Does the number of visit depend on whether we use LeastVisited/MostVisited? I guess so, so do we need two different metrics?
    def name = s"visited($kCFA)"
    type M = SequenceBasedMetric

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
          with SchemeModFSemantics
          with StandardSchemeModFComponents
          with BigStepModFSemantics
          with LeastVisitedFirstWorklistAlgorithm[SchemeExp]
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.count.values.toList)
    }
  }

  object ExpressionDepth extends Metric {
    type M = SequenceBasedMetric
    def name = "exp-depth"
    def computeDepths(exp: Expression, depths: Map[Identity, Int] = Map.empty): Map[Identity, Int] =
      exp.subexpressions.foldLeft(Map.empty[Identity, Int].withDefaultValue(0))((depths, exp) => computeDepths(exp, depths)).map({ case (k, v) => (k, v + 1) }) ++ depths + (exp.idn -> 0)
    def forProgram(program: SchemeExp): M = {
      SequenceBasedMetric(computeDepths(program).values.toList)
    }
  }

  class NumberOfDependencies(kCFA: Int) extends Metric {
    // TODO: this metric should not change for different runs on the same benchmark, nor for different strategies, but this should be checked
    type M = SequenceBasedMetric
    def name = s"dependencies($kCFA)"

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
          with SchemeModFSemantics
          with StandardSchemeModFComponents
          with BigStepModFSemantics
          with MostDependenciesFirstWorklistAlgorithm[SchemeExp]
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.depCount.values.toList)
    }
  }

  class EnvironmentSize(kCFA: Int) extends Metric {
    // TODO: this is a metric that should not change for multiple runs of the same benchmark, and should not be influenced by the strategy, but better check it
    type M = SequenceBasedMetric
    def name = s"env-size($kCFA)"

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
          with SchemeModFSemantics
          with StandardSchemeModFComponents
          with BigStepModFSemantics
          with BiggerEnvironmentFirstWorklistAlgorithm.ModF
          with SchemeModFKCallSiteSensitivity
          with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.visited.map(analysis.environmentSize).toList)
    }
  }
}

trait ParallelMetric {
  def benchmarks: Iterable[String]

  def metrics: List[Metric]

  var results = Table.empty[String].withDefaultValue("_")

  def metricForFile(file: String, metric: Metric): metric.M = {
    val program = CSchemeParser.parse(Reader.loadFile(file))
    metric.forProgram(program)
  }

  def metricsForFile(file: String): Unit =
    metrics.foreach(metric => {
      try {
        println(s"***** Computing metric ${metric.name} on $file *****")
        val result = metricForFile(file, metric)
        println(result)
        results = results.add(file, metric.name, result.toString())
      } catch {
        case e: Exception =>
          println(s"Encountered an exception: ${e.getMessage}")
        case e: VirtualMachineError =>
          System.gc()
          println(s"VM Error: ${e.getMessage}")
      }
    })

  def printResults() =
    println(results.prettyString())
  def exportCSV(path: String) = {
    val hdl = Writer.openTimeStamped(path)
    val csv = results.toCSVString()
    Writer.write(hdl, csv)
    Writer.close(hdl)
  }

  def run(path: String = "benchOutput/metrics/output.csv") = {
    benchmarks.foreach(metricsForFile)
    printResults()
    exportCSV(path)
  }
}

object ComputeParallelMetrics extends ParallelMetric {
  def k = 0
  def benchmarks = List(
    "test/R5RS/WeiChenRompf2019/meta-circ.scm",
    "test/R5RS/WeiChenRompf2019/toplas98/boyer.scm",
    "test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm",
  )
  def metrics = List(
    new Metric.CallDepth(k),
    new Metric.Visited(k),
    Metric.ExpressionDepth,
    new Metric.NumberOfDependencies(k),
    new Metric.EnvironmentSize(k)
  )


  def main(args: Array[String]) = run()
}
