package maf.modular.scv

import maf.language.scheme.*
import maf.modular.ModReporter

trait ScvReporter extends ModReporter[SchemeExp]:
    case object Z3Time extends ModMetric:
        def name: String = "z3 (ns)"
    case object SATExec extends ModMetric:
        def name: String = "# z3 executions"
    case object AppArr extends ModMetric:
        def name: String = "# monitored functions applications"
    case object AppFlat extends ModMetric:
        def name: String = "# flat contract applications"
    case object EvalCheck extends ModMetric:
        def name: String = "# check expressions"
