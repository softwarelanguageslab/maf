package maf.modular.scv

import maf.language.scheme.*
import maf.modular.worklist.*
import maf.core.Position.Position
import maf.modular.{ModAnalysis, ModReporter}

trait ScvReporter extends ModReporter[SchemeExp]:
    case object Z3Time extends ModMetric:
        def name: String = "z3 (ns)"
    case object Z3InterpreterTime extends ModMetric:
        def name: String = "z3 interpreter (ns)"
    case object SATExec extends ModMetric:
        def name: String = "# z3 executions"
    case object SATCacheHit extends ModMetric:
        def name: String = "# cache hits"
    case object AppArr extends TrackCount:
        def name: String = "# monitored functions applications"
    case object AppFlat extends ModMetric:
        def name: String = "# flat contract applications"
    case object EvalCheck extends TrackCount:
        def name: String = "# check expressions"
    case object ImplicitContract extends TrackCount:
        def name: String = "# implicit contracts"
    case object ContractCheck extends TrackCount:
        def name: String = "# explicit contracts"
    case object CollectionPhase extends ModMetric:
        def name: String = "collection phase (ms)"
    case object PropagationPhase extends ModMetric:
        def name: String = "propagation phase (ms)"

trait ScvCallSiteReporter extends ModAnalysis[SchemeExp] with ScvReporter:
    case class CallSite(to: Component, from: Position)

    abstract class DistinctCallSites extends GroupedTrackCount:
        def group(a: Any): Any = a match
            case CallSite(to, from) => to
            case _                  => throw new Exception(s"$name is not correctly tracked")

    /** Measures the distinct call sites to a component, can be used as a metric for compositionality */
    case object DistinctCallSitesMedian extends DistinctCallSites with TrackCountMedian:
        def name: String = "# distinct call sites (median)"

    case object DistinctCallSitesAverage extends DistinctCallSites with TrackCountAvg:
        def name: String = "# distinct call sites (average)"

    case object DistinctCallSitesMax extends DistinctCallSites with TrackCountMax:
        def name: String = "# distinct call sites (max)"

    case object DistinctCallSitesMin extends DistinctCallSites with TrackCountMin:
        def name: String = "# distinct call sites (min)"

    val allDistinctCallSitesMetrics = List(DistinctCallSitesMedian, DistinctCallSitesAverage, DistinctCallSitesMax, DistinctCallSitesMin)

    def trackCallSite(target: Component, loc: Position): Unit =
        allDistinctCallSitesMetrics.foreach(track(_, CallSite(target, loc)))

    def callsiteSummary: List[Int] =
        trackMetrics.get(DistinctCallSitesMedian) match
            case Some(metrics) =>
                metrics
                    .groupBy { case CallSite(to, _) =>
                        to
                    }
                    .values
                    .map(_.size)
                    .toList
            case None => List()
