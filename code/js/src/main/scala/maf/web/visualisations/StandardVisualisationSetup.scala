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
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.modular.scheme.modf.SchemeModFComponent.Call

@JSExportTopLevel("standardVisualisationSetup")
object StandardVisualisationSetup extends VisualisationSetup {

  type Analysis = WebVisualisationAnalysis[_]

  def createVisualisation(
      analysis: Analysis,
      width: Int,
      height: Int
    ) =
    new WebVisualisation(analysis, width, height).node

  def createAnalysis(text: String): Analysis = {
    val program = SchemeParser.parse(text)
    new SimpleSchemeModFAnalysis(program)
      with SchemeModFNoSensitivity
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
      with WebVisualisationAnalysis[SchemeExp] {
      override def intraAnalysis(cmp: SchemeModFComponent) =
        new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
      def moduleName(cmp: Component) = cmp match {
        case Main => "main"
        case Call(clo, _) => clo._1.lambdaName
      }
    }
  }
}
