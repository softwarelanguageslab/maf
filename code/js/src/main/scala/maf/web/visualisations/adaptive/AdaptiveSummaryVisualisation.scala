package maf.web.visualisations.adaptive

// MAF imports

import maf.core._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.modular.adaptive.scheme._
import maf.util.datastructures.MultiSet
import maf.util.benchmarks.Timeout

import maf.web.utils._
import maf.web.utils.D3Helpers._

// Scala.js imports
import org.scalajs._
import org.scalajs.dom._
import maf.modular.AddrDependency

//
// REQUIRED ANALYSIS EXTENSION
//

trait WebSummaryAdaptiveAnalysis extends AdaptiveContextSensitivity with AdaptiveAnalysisSummary:
    this: AdaptiveContextSensitivityPolicy =>

    var webSummary: AdaptiveSummaryVisualisation = _

    // checks if the adaptive analysis will adapt on its next step
    def willAdapt: Boolean = stepCount == (rate - 1)

    // refresh the summary visualisation after stepping
    override def step(timeout: Timeout.T): Unit =
        super.step(timeout)
        webSummary.refresh()

    override def adaptAnalysis(): Unit =
        super.adaptAnalysis()
        webSummary.adapt()

class AdaptiveSummaryVisualisation(
    val analysis: WebSummaryAdaptiveAnalysis,
    width: Int,
    height: Int):

    // give the adaptive analysis a pointer to this visualisation
    analysis.webSummary = this

    //
    // the state of the analysis
    //

    val node = document.createElement("div")
    d3.select(node)
      .style("overflow-x", "scroll")
      .style("width", s"${width}px")
      .style("height", s"${height}px")
    private val widthPerView = width / 3

    // TODO: this logic can be factored out in a trait for greater reuseability
    // keep a stack of views currently shown
    private var viewStack: List[View] = List.empty
    private def unrollViewStackUntil(view: View) =
      while viewStack.head != view do
          node.removeChild(viewStack.head.node)
          viewStack = viewStack.tail
    private def pushViewStack(view: View) =
        // append the node
        d3.select(view.node).style("float", "left")
        node.appendChild(view.node)
        // update the view stack
        viewStack = view :: viewStack
        // refresh the newly added view
        view.refresh()

    def refresh() = viewStack.foreach(_.refresh())
    def adapt() =
      viewStack.foreach { view =>
          view.adapt()
          view.refresh()
      }

    sealed trait View:
        val node: dom.Node
        def refresh(): Unit
        def adapt(): Unit = ()

    sealed trait BarChartView extends View { view =>
      // a bar chart that can create a new view when a bar is clicked
      abstract class NavigationBarChart(className: String) extends BarChart(widthPerView, height, padding = 50) with BarChartFocus with BarChartStats:
          // TODO: factor our "selection logic" in separate trait
          // things that can be selected in the bar chart
          sealed private trait Selection
          private case object None extends Selection
          private case object TotalSelected extends Selection
          private case object AverageSelected extends Selection
          private case class DataSelected(d: Data) extends Selection
          // keep track of the current selection
          private var currentSelection: Selection = None
          private def setup(sel: Selection) = sel match
              case None => ()
              case TotalSelected =>
                domainView.foreach(pushViewStack)
                totalText.classed("selected", true)
                focus(_ => false) // hide all the bars
              case AverageSelected =>
                averageText.classed("selected", true)
                focus(d => highlightedDataKeys.contains(key(d))) // highlight some of the bars
              case DataSelected(d) =>
                detailView(d).foreach(pushViewStack)
                focus(d)
          private def teardown() = currentSelection match
              case None => ()
              case TotalSelected =>
                unrollViewStackUntil(view)
                totalText.classed("selected", false)
                resetFocus()
              case AverageSelected =>
                averageText.classed("selected", false)
                resetFocus()
              case DataSelected(_) =>
                unrollViewStackUntil(view)
                resetFocus()
          private def setSelection(sel: Selection) =
              teardown() // teardown the current focus
              setup(sel) // setup the new focus
              currentSelection = sel
          private def toggle(sel: Selection) =
            if currentSelection == sel then setSelection(None)
            else setSelection(sel)
          // automatically provide navigation logic when clicking on a bar
          // should be implemented: what next view to produce for given data
          protected def detailView(d: Data): Option[View]
          override protected def onClick(d: Data): Unit = toggle(DataSelected(d))
          // clicking on average -> highlight bars (usually those with high values)
          // should be implemented to indicate which bars need to be highlighed
          protected def highlightedDataKeys: Set[String]
          override protected def onAverageClick(node: dom.Node) = toggle(AverageSelected)
          // clicking on total -> open new view for domain
          // should be implemented to offer a detail view for the domain
          protected def domainView: Option[View]
          override protected def onTotalClick(node: dom.Node) = toggle(TotalSelected)
          // set the correct CSS class for the barchart
          this.classed(className)
      // expects a bar chart + method to fetch the latest data
      val BarChart: NavigationBarChart
      def data: Iterable[BarChart.Data]
      // node = barchart node; refresh = barchart refresh
      val node = BarChart.node
      def refresh() = BarChart.loadDataSorted(data)
    }

    object ModuleView extends BarChartView:
        def data = analysis.summary.content
        object BarChart extends NavigationBarChart("module_bar_chart"):
            type Data = (analysis.SchemeModule, analysis.ModuleSummary)
            def key(d: Data) = d._1.toString
            def value(d: Data) = d._2.cost
            protected def detailView(d: Data) = Some(new ComponentView(d._1))
            protected def highlightedDataKeys = ???
            protected def domainView = None

    class ComponentView(module: analysis.SchemeModule) extends BarChartView:
        private def ms = analysis.summary(module)
        def data = ms.content
        object BarChart extends NavigationBarChart("component_bar_chart") with BarChartTooltip:
            type Data = (analysis.Component, MultiSet[Dependency])
            def key(d: Data) = d._1.toString
            def value(d: Data) = d._2.cardinality
            protected def tooltipText(d: Data) = analysis.view(d._1).toString
            protected def highlightedDataKeys = analysis.selectLargest(data, value, value(data.maxBy(value))).toSet.map(key)
            protected def detailView(d: Data) = Some(new DependencyView(module, d._1))
            protected def domainView = makeClosureView(module)

    class ClosureView(module: analysis.LambdaModule) extends BarChartView:
        private def cmps = analysis.summary(module).components.toSet
        def data = cmps.groupBy(cmp => analysis.view(cmp).asInstanceOf[Call[_]].clo)
        object BarChart extends NavigationBarChart("closure_bar_chart") with BarChartTooltip:
            type Data = (analysis.lattice.Closure, Set[analysis.Component])
            def key(d: Data) =
                val (lambda, env) = d._1
                s"${lambda.lambdaName} [${env.asInstanceOf[WrappedEnv[_, _]].data}]"
            def value(d: Data) = d._2.size
            protected def tooltipText(d: Data) = d._2.map(analysis.view(_).toString).mkString("<br>")
            protected def detailView(d: Data) = None
            protected def highlightedDataKeys = analysis.selectLargest(data, value, value(data.maxBy(value))).toSet.map(key)
            protected def domainView = makeClosureView(analysis.getParentModule(data.head._1))
    private def makeClosureView(module: analysis.SchemeModule): Option[ClosureView] = module match
        case lam: analysis.LambdaModule => Some(new ClosureView(lam))
        case _                          => None

    class DependencyView(module: analysis.SchemeModule, var component: analysis.Component) extends BarChartView:
        private def ms = analysis.summary(module)(component)
        def data = ms.content
        override def adapt() = component = analysis.adaptComponent(component)
        object BarChart extends NavigationBarChart("dependency_bar_chart") with BarChartTooltip:
            type Data = (Dependency, Int)
            private def toAddr(dep: Dependency): Address = dep match
                case AddrDependency(addr) => addr
                case _                    => throw new Exception(s"Unknown dependency $dep")
            def key(d: Data) = toAddr(d._1) match
                case PrmAddr(name)      => s"PrmAddr($name)"
                case VarAddr(id, ctx)   => s"VarAddr(${id.name}@${id.idn.pos}) [$ctx]"
                case PtrAddr(exp, ctx)  => s"PtrAddr($exp@${exp.idn.pos}) [$ctx]"
                case ReturnAddr(cmp, _) => s"RetAddr($cmp)"
            def value(d: Data) = d._2
            protected def tooltipText(d: Data) = analysis.store(toAddr(d._1)).toString
            protected def detailView(d: Data) = None
            protected def highlightedDataKeys = analysis.selectLargest(data, value, value(data.maxBy(value))).toSet.map(key)
            protected def domainView = None

    // initialise by showing the module view
    pushViewStack(ModuleView)
