package maf.web.visualisations

import maf.modular.scheme.modflocal._
import maf.modular.worklist._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.language.scheme._

import scala.scalajs.js.annotation._
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.modular.scheme.modf.SchemeModFComponent.Call

class SimpleWebVisualisation(
    override val analysis: WebVisualisationAnalysis[_],
    width: Int,
    height: Int)
    extends WebVisualisation(width, height)

trait StandardVisualisationSetup extends VisualisationSetup:

    type Analysis = WebVisualisationAnalysis[_]

    def createVisualisation(
        analysis: Analysis,
        width: Int,
        height: Int
      ) =
      new SimpleWebVisualisation(analysis, width, height).node

@JSExportTopLevel("standardModFVisualisationSetup")
object StandardModFVisualisationSetup extends StandardVisualisationSetup:

    def createAnalysis(text: String): Analysis =
        val program = SchemeParser.parseProgram(text)
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFNoSensitivity
          with SchemeConstantPropagationDomain
          with FIFOWorklistAlgorithm[SchemeExp]
          with WebVisualisationAnalysis[SchemeExp] {
          override def intraAnalysis(cmp: SchemeModFComponent) =
            new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
          // TODO: move this to somewhere in `maf.modular` directly
          type Module = Option[SchemeLambdaExp]
          def module(cmp: Component) = cmp match
              case Main                 => None
              case Call((lambda, _), _) => Some(lambda)
          def moduleName(mdl: Module) = mdl.map(_.lambdaName).getOrElse("main")
        }

@JSExportTopLevel("standardModFLocalVisualisationSetup")
object StandardModFLocalVisualisationSetup extends StandardVisualisationSetup:
    def createAnalysis(txt: String): Analysis = ???
