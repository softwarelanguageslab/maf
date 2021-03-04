package maf.web.visualisations

import maf.web._

import maf.core._
import maf.modular._
import maf.modular.worklist._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.language.scheme._
import maf.util.benchmarks.Timeout

import scala.scalajs.js.annotation._

@JSExportTopLevel("standardVisualisationSetup")
object StandardVisualisationSetup extends VisualisationSetup {

  def createVisualisation(text: String) =
    new WebVisualisation(newAnalysis(text))

  private def newAnalysis(text: String) = {
    val program = SchemeParser.parse(text)
    new SimpleSchemeModFAnalysis(program)
      with SchemeModFNoSensitivity
      with SchemeConstantPropagationDomain
      with DependencyTracking[SchemeExp]
      with FIFOWorklistAlgorithm[SchemeExp] {

      def key(cmp: Component): Identity = expr(cmp).idn

      override def step(t: Timeout.T) = {
        val cmp = workList.head
        println(cmp)
        super.step(t)
      }

      override def intraAnalysis(cmp: SchemeModFComponent) =
        new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
    }
  }
}
