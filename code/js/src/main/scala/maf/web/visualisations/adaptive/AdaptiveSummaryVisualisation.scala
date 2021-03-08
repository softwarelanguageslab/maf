package maf.web.visualisations.adaptive

// MAF imports
import maf.modular.adaptive.scheme._

// Scala.js imports
import maf.util.benchmarks.Timeout

import maf.web.utils.BarChart

//
// REQUIRED ANALYSIS EXTENSION
//

trait WebSummaryAdaptiveAnalysis extends AdaptiveContextSensitivity {

  var webSummary: AdaptiveSummaryVisualisation = _

  override def step(timeout: Timeout.T): Unit = {
    super.step(timeout)
    webSummary.refresh()
  }
}

class AdaptiveSummaryVisualisation(val analysis: WebSummaryAdaptiveAnalysis, width: Int, height: Int) {

  // give the adaptive analysis a pointer to this visualisation
  analysis.webSummary = this

  //
  // setting up the visualisation
  //

  object ModuleCostBarChart extends BarChart(width, height) {
    type Data = (analysis.SchemeModule, analysis.ModuleSummary)
    def key(d: Data): String = d._1.toString
    def value(d: Data): Int = d._2.cost
  }

  val node = ModuleCostBarChart.node

  //
  // coordinating the visualisation
  //

  def refresh(): Unit = 
    ModuleCostBarChart.loadDataSorted(analysis.summary.content)

  refresh()
}
