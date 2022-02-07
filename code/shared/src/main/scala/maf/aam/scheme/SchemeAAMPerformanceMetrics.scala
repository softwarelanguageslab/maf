package maf.aam.scheme

import scala.collection.mutable.Map
import maf.aam.AAMAnalysis
import maf.core.Expression

/** Trait that provides instrumentation to report various metrics of the analysis */
trait AAMPeformanceMetrics[E <: Expression] extends AAMAnalysis[E]:
    sealed abstract class Metric:
        def name: String

    case object Seen extends Metric:
        def name: String = "seen"

    case object Bump extends Metric:
        def name: String = "bumps"

    private val collectedMetrics: Map[Metric, Double] = Map().withDefault(_ => 0.0)

    protected inline def report(metricType: Metric, value: Double): Unit =
      collectedMetrics.update(metricType, value)

    protected inline def increment(metricType: Metric): Unit =
      collectedMetrics.updateWith(metricType) {
        case Some(old) => Some(old + 1)
        case None      => Some(1)
      }

    override def metrics: List[maf.modular.Metric] = List()

    def reportMetrics: List[(String, Double)] =
      collectedMetrics.map { case (key, v) =>
        (key.name, v)
      }.toList
