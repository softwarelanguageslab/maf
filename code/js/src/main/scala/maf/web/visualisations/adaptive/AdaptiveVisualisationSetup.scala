package maf.web.visualisations.adaptive

// MAF imports
import maf.core._
import maf.modular._
import maf.modular.worklist._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.language.scheme._
import maf.util.benchmarks.Timeout

import maf.web._
import maf.web.utils._
import maf.web.utils.D3Helpers._

// Scala.js related imports
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom._
import maf.web.visualisations.VisualisationSetup
import maf.modular.components.ComponentPointer

//
// VISUALISATION SETUP
//

@JSExportTopLevel("adaptiveVisualisationSetup")
object AdaptiveVisualisationSetup extends VisualisationSetup {

  type Analysis = WebVisualisationAnalysisAdaptive[SchemeExp] with WebSummaryAdaptiveAnalysis

  def createAnalysis(text: String) = {
    val prg = SchemeParser.parse(text)
    new AdaptiveModAnalysis(prg)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      with WebVisualisationAnalysisAdaptive[SchemeExp]
      with WebSummaryAdaptiveAnalysis {

      override def intraAnalysis(cmp: Component) = new AdaptiveSchemeModFIntra(cmp) with DependencyTrackingIntra
      def key(cmp: Component) = expr(cmp)

      // setup the budget
      lazy val budget = 100
      // log every step in the console
      var step = 0
      override def step(timeout: Timeout.T): Unit = {
        val cmp = workList.head
        println(s"[$step] Analysing ${view(cmp)}")
        step += 1
        super.step(timeout)
      }
    }
  }

  def createVisualisation(
      analysis: Analysis,
      width: Int,
      height: Int
    ): Node = {
    // create both a webvis and a summary vis
    val webWidth = Math.round(width * 0.6)
    val sumWidth = Math.round(width * 0.35)
    val webvis = new WebVisualisationAdaptive(analysis, webWidth.toInt, height)
    val sumvis = new AdaptiveSummaryVisualisation(analysis, sumWidth.toInt, height)
    // create the parent
    val parent = document.createElement("div")
    d3.select(webvis.node).style("float", "left")
    d3.select(sumvis.node).style("float", "left")
    parent.appendChild(webvis.node)
    parent.appendChild(sumvis.node)
    return parent
  }

  //
  // INPUT HANDLING
  //

  override def onClick(): Unit = () // don't do anything when clicking

  override def analysisCommandHandler(anl: Analysis) =
    analysisCommandHandlerAdaptive(anl).orElse(super.analysisCommandHandler(anl))

  private def analysisCommandHandlerAdaptive(anl: Analysis): PartialFunction[String, Unit] = { case "a" | "A" =>
    stepUntilAdapt(anl)
  }

  private def stepUntilAdapt(anl: Analysis): Unit =
    if (!anl.finished && !anl.willAdapt) {
      anl.step(Timeout.none)
      js.timers.setTimeout(0) {
        stepUntilAdapt(anl)
      }
    }
}
