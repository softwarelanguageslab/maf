package maf.web.visualisations.adaptive

import maf.core._
import maf.modular._
import maf.modular.adaptive._
import maf.util.MonoidImplicits._

import maf.web.visualisations._

// Scala.js-related imports
import scala.scalajs.js

object WebVisualisationAdaptive:
    val d3 = js.Dynamic.global.d3
    lazy val __NODE_COLORS__ = List("blue", "green", "yellow", "red")
    lazy val __NO_OF_COLORS__ = __NODE_COLORS__.length
    lazy val __COLOR_SCALE__ = d3
      .scaleOrdinal()
      .domain(d3.range(__NO_OF_COLORS__))
      .range(__NODE_COLORS__)

//
// REQUIRED ANALYSIS EXTENSION
//

trait WebVisualisationAnalysisAdaptive[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] with WebVisualisationAnalysis[Expr]:

    def adaptiveWebVis = webvis.asInstanceOf[WebVisualisationAdaptive]

    override def adaptAnalysis() =
        super.adaptAnalysis()
        this.dependencies = adaptMap(adaptComponent, adaptSet(adaptComponent))(this.dependencies)
        adaptiveWebVis.dirty = true // ensure the webvis will refresh its data

    override def intraAnalysis(cmp: Component): IntraAnalysis with GlobalStoreIntra with DependencyTrackingIntra

//
// WEB VISUALISATION FOR ADAPTIVE ANALYSES
//

class WebVisualisationAdaptive(
    override val analysis: WebVisualisationAnalysisAdaptive[_],
    width: Int,
    height: Int)
    extends WebVisualisation(width, height):

    // dirty flag set after visualisation needs to be refreshed due to adaptation
    var dirty = false

    override def componentKey(cmp: analysis.Component) = analysis.module(cmp)
    override def componentText(cmp: analysis.Component) = s"$cmp"

    override def refreshDataAfterStep() =
      if this.dirty then super.refreshData() // <- do a full refresh
      else super.refreshDataAfterStep()

    override def refreshData() =
        super.refreshData()
        this.dirty = false
