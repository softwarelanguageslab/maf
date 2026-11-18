package maf.analysis

import maf.domain.*
import maf.fixpoint.Analysis
import maf.syntax.*

/** An analysis that records the values that it writes to each position (e.g., to compare them with the values of a concrete interpreter). */
trait AnalysisResults[Expr <: Expression] extends Analysis[?] with AbstractDomain[Expr]:

    /** Returns the values that the analysis wrote to each position, according to the solution `sol`. */
    def resultsPerPos(sol: Solution): Map[Option[Position], Set[Value]]
