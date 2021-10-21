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

trait Metric:
    def name: String
    def forProgram(program: SchemeExp): Metric.SequenceBasedMetric

object Metric:
    case class SequenceBasedMetric(vs: List[Int]):
        def mean = Statistics.mean(vs.map(_.toDouble)).toInt
        def median = Statistics.median(vs.map(_.toDouble)).toInt
        def stddev = Statistics.stddev(vs.map(_.toDouble)).toInt
        def max = vs.max.toInt
        def add(v: Int): SequenceBasedMetric = SequenceBasedMetric(v :: vs)
        override def toString = s"$mean [$median±$stddev] <= $max"

    object ExpressionDepth extends Metric:
        type M = SequenceBasedMetric
        def name = "exp-depth"
        def computeDepths(exp: Expression, depths: Map[Identity, Int] = Map.empty): Map[Identity, Int] =
          exp.subexpressions
            .foldLeft(Map.empty[Identity, Int].withDefaultValue(0))((depths, exp) => computeDepths(exp, depths))
            .map({ case (k, v) => (k, v + 1) }) ++ depths + (exp.idn -> 0)
        def forProgram(program: SchemeExp): M =
          SequenceBasedMetric(computeDepths(program).values.toList)

    class CallDepth(val kCFA: Int) extends Metric:
        def name = s"call-depth"
        type M = SequenceBasedMetric

        def forProgram(program: SchemeExp): M =
            val analysis = new ModAnalysis(program)
              with SchemeModFSemanticsM
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

    class LeastVisited(kCFA: Int) extends Metric:
        def name = s"least-visited"
        type M = SequenceBasedMetric

        def forProgram(program: SchemeExp): M =
            val analysis = new ModAnalysis(program)
              with SchemeModFSemanticsM
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
    class MostVisited(kCFA: Int) extends Metric:
        def name = s"most-visited"
        type M = SequenceBasedMetric

        def forProgram(program: SchemeExp): M =
            val analysis = new ModAnalysis(program)
              with SchemeModFSemanticsM
              with StandardSchemeModFComponents
              with BigStepModFSemantics
              with MostVisitedFirstWorklistAlgorithm[SchemeExp]
              with SchemeModFKCallSiteSensitivity
              with SchemeConstantPropagationDomain {
              val k = kCFA
              override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
            }
            analysis.analyze()
            SequenceBasedMetric(analysis.count.values.toList)

    class NumberOfDependencies(kCFA: Int) extends Metric:
        type M = SequenceBasedMetric
        def name = s"deps"

        def forProgram(program: SchemeExp): M =
            val analysis = new ModAnalysis(program)
              with SchemeModFSemanticsM
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

    class EnvironmentSize(kCFA: Int) extends Metric:
        type M = SequenceBasedMetric
        def name = s"env-size"

        def forProgram(program: SchemeExp): M =
            val analysis = new ModAnalysis(program)
              with SchemeModFSemanticsM
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

trait Metrics:
    def benchmarks: Iterable[String]

    def metrics: List[Metric]

    var results = Table.empty[Metric.SequenceBasedMetric].withDefaultValue(Metric.SequenceBasedMetric(List()))

    def metricForFile(file: String, metric: Metric): Metric.SequenceBasedMetric =
        val program = CSchemeParser.parseProgram(Reader.loadFile(file))
        metric.forProgram(program)

    def metricsForFile(file: String): Unit =
      metrics.foreach { metric =>
        try
            println(s"***** Computing metric ${metric.name} on $file *****")
            val result = metricForFile(file, metric)
            println(result)
            results = results.add(file, metric.name, result)
        catch
            case e: Exception =>
              println(s"Encountered an exception: ${e.getMessage}")
            case e: VirtualMachineError =>
              System.gc()
              println(s"VM Error: ${e.getMessage}")
      }

    def printResults() =
      println(results.prettyString())
    def exportCSV(
        path: String,
        format: Metric.SequenceBasedMetric => String,
        timestamped: Boolean = true
      ) =
        val hdl = if timestamped then Writer.openTimeStamped(path) else Writer.open(path)
        val csv = results.toCSVString(format = format)
        Writer.write(hdl, csv)
        Writer.close(hdl)

    def run(path: String = "benchOutput/metrics/output.csv") =
        benchmarks.foreach(metricsForFile)
        printResults()
        exportCSV(path, format = _.toString())

trait ParallelMetrics extends Metrics:
    def k: Int
    def metrics = List(
      Metric.ExpressionDepth,
      new Metric.CallDepth(k),
      new Metric.LeastVisited(k),
      new Metric.MostVisited(k),
      new Metric.NumberOfDependencies(k),
      new Metric.EnvironmentSize(k)
    )
    def formatMean(m: Metric.SequenceBasedMetric): String = m.mean.toString()
    def formatStddev(m: Metric.SequenceBasedMetric): String = m.stddev.toString()
    def formatMax(m: Metric.SequenceBasedMetric): String = m.max.toString()
    def main(args: Array[String]): Unit =
        run()
        exportCSV("data/modf-context-insensitive-metrics-mean.csv", formatMean _, timestamped = false)
        exportCSV("data/modf-context-insensitive-metrics-stddev.csv", formatStddev _, timestamped = false)
        exportCSV("data/modf-context-insensitive-metrics-max.csv", formatMax _, timestamped = false)
        ParallelModFBenchmarks.all.foreach { (benchmark: String) =>
            val shortName = ParallelModFBenchmarks.paperName(benchmark)
            val expDepth = results.get(benchmark, "exp-depth").get
            val callDepth = results.get(benchmark, "call-depth").get
            val leastVisited = results.get(benchmark, "least-visited").get
            val mostVisited = results.get(benchmark, "most-visited").get
            val deps = results.get(benchmark, "deps").get
            val envSize = results.get(benchmark, "env-size").get
            // TODO: find a way to have an understandable formatting for the metrics
            println(s"\\prog{$shortName} & $expDepth & $callDepth & $leastVisited & $mostVisited & $deps & $envSize \\\\")
        }

object ParallelMetrics0CFA extends ParallelMetrics:
    def k = 0
    def benchmarks = ParallelModFBenchmarks.all

object ParallelMetrics2CFA extends ParallelMetrics:
    def k = 2
    def benchmarks = ParallelModFBenchmarks.for2CFA
