package maf.analysis

import maf.domain.*
import maf.syntax.*

trait AnalysisResults[Expr <: Expression] extends AbstractDomain[Expr]:
    def resultsPerPos: Map[Option[Position], Set[Value]]
