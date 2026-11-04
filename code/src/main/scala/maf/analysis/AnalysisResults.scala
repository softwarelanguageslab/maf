package maf.analysis

import maf.domain.*
import maf.syntax.*

trait AnalysisResults[Expr <: Expression] extends AbstractDomain[Expr]:
    def resultsPerIdn: Map[Identity, Set[Value]]
