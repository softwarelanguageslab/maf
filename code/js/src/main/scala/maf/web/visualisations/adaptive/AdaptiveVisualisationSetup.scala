package maf.web.visualisations.adaptive

// MAF imports
import maf.modular.worklist._
import maf.modular.scheme._
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.language.scheme._
import maf.util.benchmarks.Timeout

import maf.web.utils._

// Scala.js related imports
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom._
import maf.web.visualisations._

//
// VISUALISATION SETUP
//

@JSExportTopLevel("adaptiveVisualisationSetup")
object AdaptiveVisualisationSetup extends VisualisationSetup:

    type Analysis = WebVisualisationAnalysisAdaptive[SchemeExp] with WebSummaryAdaptiveAnalysis

    def createAnalysis(text: String) =
        val prg = SchemeParser.parseProgram(text)
        new AdaptiveModAnalysis(prg, rate = 100)
          with AdaptiveSchemeModFSemantics
          with AdaptiveContextSensitivity
          with SchemeConstantPropagationDomain
          with FIFOWorklistAlgorithm[SchemeExp]
          with WebVisualisationAnalysisAdaptive[SchemeExp]
          with WebSummaryAdaptiveAnalysis
          with AdaptiveKCFA {

          override def intraAnalysis(cmp: Component) =
            new AdaptiveSchemeModFIntra(cmp) with DependencyTrackingIntra

          type Module = SchemeModule
          def moduleName(mdl: Module) = mdl.toString

          // setup the budget
          lazy val n = 100
          lazy val t = 100
          // log every step in the console
          var step = 0
          override def step(timeout: Timeout.T): Unit =
              val cmp = workList.head
              step += 1
              println(s"[$step] Analysing ${view(cmp)}")
              super.step(timeout)
        }

    def createVisualisation(
        analysis: Analysis,
        width: Int,
        height: Int
      ): Node =
        // create both a webvis and a summary vis
        val sumvis = new AdaptiveSummaryVisualisation(analysis, width, (0.75 * height).toInt)
        val webvis = new WebVisualisationAdaptive(analysis, width, height) with WebVisualisationWithToggle
        // create the parent
        VStack(sumvis.node, webvis.node)

    //
    // INPUT HANDLING
    //

    override def onClick(): Unit = () // don't do anything when clicking

    override def analysisCommandHandler(anl: Analysis) =
      analysisCommandHandlerAdaptive(anl).orElse(super.analysisCommandHandler(anl))

    private def analysisCommandHandlerAdaptive(anl: Analysis): PartialFunction[String, Unit] =
        case "a" | "A" => anl.adaptAnalysis()
        case "c" | "C" => stepUntilAdapt(anl)

    private def stepUntilAdapt(anl: Analysis): Unit =
      if !anl.finished && !anl.willAdapt then
          anl.step(Timeout.none)
          js.timers.setTimeout(0) {
            stepUntilAdapt(anl)
          }
