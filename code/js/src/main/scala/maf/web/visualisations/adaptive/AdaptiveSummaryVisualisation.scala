package maf.web.visualisations.adaptive

// MAF imports
import maf.modular.adaptive.scheme._
import maf.util.datastructures.MultiSet
import maf.util.benchmarks.Timeout

import maf.web.utils.BarChart
import maf.modular.Dependency

// Scala.js imports
import org.scalajs._
import org.scalajs.dom._

//
// REQUIRED ANALYSIS EXTENSION
//

trait WebSummaryAdaptiveAnalysis extends AdaptiveContextSensitivity {

  var webSummary: AdaptiveSummaryVisualisation = _

  // checks if the adaptive analysis will adapt on its next step
  def willAdapt: Boolean = stepCount == rate 

  // refresh the summary visualisation after stepping
  override def step(timeout: Timeout.T): Unit = {
    super.step(timeout)
    webSummary.refresh()
  }
}

class AdaptiveSummaryVisualisation(
    val analysis: WebSummaryAdaptiveAnalysis,
    width: Int,
    height: Int) {

  // give the adaptive analysis a pointer to this visualisation
  analysis.webSummary = this

  //
  // the state of the analysis
  //

  sealed trait View {
    def node: dom.Node
    def refresh(): Unit
  }
  case object ModuleView extends View {
    def node = ModuleBarChart.node
    def refresh() = ModuleBarChart.loadDataSorted(analysis.summary.content)
  }
  case class ComponentView(ms: analysis.ModuleSummary) extends View {
    def node = ComponentBarChart.node
    def refresh() = ComponentBarChart.loadDataSorted(ms.content)
  }
  case class DependencyView(ms: MultiSet[Dependency]) extends View {
    def node = DependencyBarChart.node
    def refresh() = DependencyBarChart.loadDataSorted(ms.content)
  }

  // initially, just show the module view
  private var currentView: View = ModuleView

  //
  // setting up the bar charts
  //

  object ModuleBarChart extends BarChart(width, height) {
    type Data = (analysis.SchemeModule, analysis.ModuleSummary)
    def key(d: Data): String = d._1.toString
    def value(d: Data): Int = d._2.cost
  }

  object ComponentBarChart extends BarChart(width, height) {
    type Data = (analysis.Component, MultiSet[Dependency])
    def key(d: Data): String = d._1.toString
    def value(d: Data): Int = d._2.cardinality
  }

  object DependencyBarChart extends BarChart(width, height) {
    type Data = (Dependency, Int)
    def key(d: Data): String = d._1.toString
    def value(d: Data): Int = d._2
  }

  //
  // setting up the visualisation
  //

  val node = document.createElement("div")
  node.appendChild(currentView.node)

  def switchView(view: View) {
    node.removeChild(currentView.node)
    currentView = view
    node.appendChild(currentView.node)
  }

  //
  // coordinating the visualisation
  //

  def refresh(): Unit = currentView.refresh()
  
  refresh()
}