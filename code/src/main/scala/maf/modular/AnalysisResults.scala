package maf.modular

import maf.core._

trait AnalysisResults[Expr <: Expression] extends AbstractDomain[Expr]:
    def resultsPerIdn: Map[Identity, Set[Value]]
