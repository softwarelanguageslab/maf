package maf.web.visualisations.adaptive

// MAF imports
import maf.modular.Dependency
import maf.modular.adaptive.scheme._
import maf.util.datastructures.MultiSet
import maf.util.benchmarks.Timeout

import maf.web.utils._

// Scala.js imports
import org.scalajs._
import org.scalajs.dom._
import maf.modular.AddrDependency

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

  val node = document.createElement("div")
  private val widthPerView = (width / 3) - 10

  // keep a stack of views currently shown
  private var viewStack: List[View] = List.empty
  private def unrollViewStackUntil(view: View) = {
    while (viewStack.head != view) {
      node.removeChild(viewStack.head.node)
      viewStack = viewStack.tail
    }
  }
  private def pushViewStack(view: View) = {
    // append the node
    D3Helpers.d3.select(view.node).style("float", "left")
    node.appendChild(view.node)
    // update the view stack
    viewStack = view :: viewStack
    // refresh the newly added view
    view.refresh()
  }

  def refresh() = viewStack.foreach(_.refresh())

  sealed trait View {
    val node: dom.Node
    def refresh(): Unit
  }

  sealed trait BarChartView extends View { view =>
    // a bar chart that can create a new view when a bar is clicked
    abstract class NavigationBarChart extends BarChart(widthPerView, height) with BarChartFocus {
      // should be implemented: what next view to produce for given data
      protected def childView(d: Data): Option[View]
      // automatically provide navigation logic when clicking on a bar
      private var selected: Option[Data] = None
      override protected def onClick(d: Data): Unit = {
        // remove children views (if any)
        unrollViewStackUntil(view)
        // if clicking on the current selection, remove the selection and its children
        if (selected.isDefined && d == selected.get) {
          selected = None
          resetFocus()
        } else { // otherwise, just add the next view
          selected = Some(d)
          focus(_ == d)
          childView(d).foreach { child =>
            pushViewStack(child)
          }
        }
      }
    }
    // expects a bar chart + method to fetch the latest data
    val BarChart: NavigationBarChart
    def data: Iterable[BarChart.Data]
    // node = barchart node; refresh = barchart refresh
    val node = BarChart.node
    def refresh() = BarChart.loadDataSorted(data)
  }

  object ModuleView extends BarChartView {
    def data = analysis.summary.content
    object BarChart extends NavigationBarChart {
      type Data = (analysis.SchemeModule, analysis.ModuleSummary)
      def key(d: Data): String = d._1.toString
      def value(d: Data): Int = d._2.cost
      protected def childView(d: Data): Option[View] = Some(new ComponentView(d._1))
    }
    // give this bar chart a specific CSS class
    BarChart.classed("module_bar_chart")
  }

  class ComponentView(module: analysis.SchemeModule) extends BarChartView {
    def data = analysis.summary(module).content
    object BarChart extends NavigationBarChart with BarChartTooltip {
      type Data = (analysis.Component, MultiSet[Dependency])
      def key(d: Data): String = d._1.toString
      def value(d: Data): Int = d._2.cardinality
      protected def childView(d: Data): Option[View] = Some(new DependencyView(module, d._1))
      protected def tooltipText(d: Data) = analysis.view(d._1).toString
    }
    // give this bar chart a specific CSS class
    BarChart.classed("component_bar_chart")
  }

  class DependencyView(module: analysis.SchemeModule, component: analysis.Component) extends BarChartView {
    def data = analysis.summary(module)(component).content
    object BarChart extends NavigationBarChart with BarChartTooltip {
      type Data = (Dependency, Int)
      def key(d: Data): String = d._1.toString
      def value(d: Data): Int = d._2
      protected def childView(d: Data) = None
      protected def tooltipText(d: Data) = d._1 match {
        case AddrDependency(addr) => analysis.store(addr).toString
        case d                    => throw new Exception(s"Unknown dependency $d")
      }
    }
    // give this bar chart a specific CSS class
    BarChart.classed("dependency_bar_chart")
  }

  // initialise by showing the module view
  pushViewStack(ModuleView)
}
