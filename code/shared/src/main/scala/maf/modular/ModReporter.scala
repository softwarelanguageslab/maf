package maf.modular

import maf.language.scheme.*
import maf.util.benchmarks.Timer
import maf.core.Expression

/** A reporter to report metrics, it also includes some timing utilities to measure how long certain parts of the code take */
trait ModReporter[E <: Expression] extends AnalysisEntry[E]:
    /** The type of the metric reported by the ModF analysis */
    protected trait ModMetric:
        def name: String

    case object NumberOfComponents extends ModMetric:
        def name: String = "# components"

    case object NumberOfIntra extends ModMetric:
        def name: String = "# intra analyses"

    /** Keeps track of all reported metrics */
    private var reportedMetrics: List[(ModMetric, Double)] = List()

    /** Keeps track of a unique count */
    protected var trackMetrics: Map[ModMetric, Set[Any]] = Map()

    private var countMetrics: Map[ModMetric, Long] = Map()

    override def metrics: List[Metric] =
        reportedMetrics.map { case (k, v) => Metric(k.name, v) }.toList ++
            trackMetrics.map { case (k, v) => Metric(k.name, v.size.toDouble) }.toList ++
            countMetrics.map { case (k, v) => Metric(k.name, v.toDouble) }.toList

    def count(metric: ModMetric): Unit =
        countMetrics = countMetrics + (metric -> (countMetrics.get(metric).getOrElse(0L) + 1))

    def accumulate(metric: ModMetric, vlu: Long): Unit =
        countMetrics = countMetrics + (metric -> (countMetrics.get(metric).getOrElse(0L) + vlu))

    def track(metric: ModMetric, vlu: Any): Unit =
        trackMetrics = trackMetrics + (metric -> (trackMetrics.get(metric).getOrElse(Set()) + vlu))

    def report(metric: ModMetric, vlu: Double): Unit = reportedMetrics = (metric, vlu) :: reportedMetrics

    def time[X](metric: ModMetric)(blk: => X): X =
        val (diff, res) = Timer.time(blk)
        accumulate(metric, diff)
        res
