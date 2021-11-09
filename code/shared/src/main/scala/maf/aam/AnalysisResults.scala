package maf.aam

import maf.core.*

trait AnalysisResults extends maf.aam.AAMAnalysis:
    def resultsPerIdn: Map[Identity, Set[LatVal]]
