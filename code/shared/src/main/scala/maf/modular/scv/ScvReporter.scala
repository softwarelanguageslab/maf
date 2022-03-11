package maf.modular.scv

import maf.language.scheme.*
import maf.modular.worklist.*
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
    case object AppArr extends ModMetric:
        def name: String = "# monitored functions applications"
    case object AppFlat extends ModMetric:
        def name: String = "# flat contract applications"
    case object EvalCheck extends ModMetric:
        def name: String = "# check expressions"
    case object ImplicitContract extends ModMetric:
        def name: String = "# implicit contracts"
