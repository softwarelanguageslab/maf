package maf.web

import maf.core.Expression
import maf.modular.DependencyTracking
import maf.modular.incremental.IncrementalModAnalysis

class WebVisualisationIncremental[Expr <: Expression](override val analysis: IncrementalModAnalysis[Expr] with DependencyTracking[Expr])
  extends WebVisualisation(analysis) {

}
