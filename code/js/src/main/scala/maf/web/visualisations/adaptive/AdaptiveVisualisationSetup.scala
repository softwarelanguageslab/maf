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

// Scala.js related imports
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom._

//
// VISUALISATION SETUP
//

@JSExportTopLevel("adaptiveVisualisationSetup")
object AdaptiveVisualisationSetup {

  @JSExport
  def init() = {
    val input = FileInputElement(loadFile)
    document.body.appendChild(input)
  }

  private def loadFile(text: String) = {
    // create the analysis
    val prg = SchemeParser.parse(text)
    val analysis = new AdaptiveModAnalysis(prg)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp] {
      //with WebVisualisationAdaptiveAnalysis[SchemeExp] {
      //with WebSummaryAdaptiveAnalysis {
      lazy val budget = 100
      def key(cmp: Component) = module(cmp)
      //override def intraAnalysis(cmp: Component) = new AdaptiveSchemeModFIntra(cmp) with DependencyTrackingIntra
      var step = 0
      override def step(timeout: Timeout.T): Unit = {
        val cmp = workList.head
        println(s"[$step] Analysing ${view(cmp)}")
        step += 1
        super.step(timeout)
      }
    }
    // load the analysis
    analysis.analyze()
    val barChart = new BarChart(800, 600) {
      type Data = (analysis.SchemeModule, analysis.ModuleSummary)
      def key(d: Data) = d._1.toString
      def value(d: Data) = d._2.cost
    }
    barChart.loadDataSorted(analysis.summary.content)
    document.body.appendChild(barChart.node)
  }

}
