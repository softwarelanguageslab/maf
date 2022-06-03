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

    /** Measures the distinct call sites to a component, can be used as a metric for compositionality */
    case object DistinctCallSites extends TrackCountMedian:
        def name: String = "# distinct call sites (median)"
        def group(a: Any): Any = a match
            case CallSite(to, from) => to
            case _                  => throw new Exception(s"$name is not correctly tracked")
