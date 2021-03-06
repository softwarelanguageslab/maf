package maf.web.visualisations.adaptive

// MAF imports
import maf.modular.adaptive.scheme.AdaptiveContextSensitivity

// Scala.js imports
import org.scalajs.dom
import maf.util.benchmarks.Timeout

import maf.web.utils.D3Helpers._
import maf.web.utils.JSHelpers._

//
// REQUIRED ANALYSIS EXTENSION 
//

trait WebSummaryAdaptiveAnalysis extends AdaptiveContextSensitivity {

    var webSummary: AdaptiveSummaryVisualisation = _ 

    private var modules: Set[SchemeModule] = Set(MainModule)
    override def spawn(cmp: Component): Unit = {
        super.spawn(cmp)
        // add to the set of modules if necessary
        val m = module(cmp)
        if(!modules(m)) {
            modules += m
            webSummary.onModuleAdded()
        }
    }

    override def step(timeout: Timeout.T): Unit = {
        super.step(timeout)
        webSummary.onStepped()
    }
}

class AdaptiveSummaryVisualisation(val analysis: WebSummaryAdaptiveAnalysis) {

    // give the adaptive analysis a pointer to this visualisation
    analysis.webSummary = this

    //
    // coordinating the visualisation
    //

    trait VisualisationState
    case object Idle extends VisualisationState
    case class ModulesOverview(xScale: JsAny, yScale: JsAny) extends VisualisationState

    var state: VisualisationState = Idle 

    def onModuleAdded(): Unit = {}
    def onStepped(): Unit = {}
    
    def init(parent: dom.Node, width: Int, height: Int, margin: Int) = {
        assert(state == Idle)
        val svg = d3.select(parent).append("svg")
                    .attr("width", width + 2 * margin)
                    .attr("height", height + 2 * margin)
        val xScale = d3.scaleBand().range(Seq(0, width)).padding(0.4)
        val yScale = d3.scaleLinear().range(Seq(height, 0))
        val g = svg.append("g").attr("transform", s"translate($margin,$margin)")
        // setup the x axis
        val xAxis = g.append("g")
                        .attr("transform", s"translate(0,$height)")
                        .call(d3.axisBottom(xScale));
        // setup the y axis
        /*
        val yAxis = g.append("g")
                        .call(d3.axisLeft(yScale).ticks(10))
                        .append("text")
                        .attr("y", 6)
                        .attr("dy", "0.71em")
                        .attr("text-anchor", "end")
        */
        // update the state
        //state = ModulesOverview(xAxis, yAxis)
    }
}