package maf.web.visualisations

import maf.modular._
import maf.modular.worklist._
import maf.util.benchmarks.Timeout
import scala.concurrent.duration._

import org.scalajs._
import org.scalajs.dom._
import maf.web.utils._
import maf.web.utils.D3Helpers._
import scala.scalajs.js.annotation._
import scala.scalajs.js

trait VisualisationSetup {

  type Analysis <: ModAnalysis[_] with SequentialWorklistAlgorithm[_]

  var currentAnl: Option[Analysis] = None
  var currentVis: Option[dom.Node] = None

  // create some visualisation for the given program (with given dimensions)
  def createAnalysis(program: String): Analysis
  def createVisualisation(analysis: Analysis, width: Int, height: Int): dom.Node

  @JSExport
  def setup() = {
    // add an element to select a file
    val input = FileInputElement(loadFile)
    document.body.appendChild(input)
    // input handling
    val body = d3.select(document.body)
    body.on("keypress", () => keyHandler.lift(d3.event.key.asInstanceOf[String]))
    body.on("click", () => onClick())
  }

  private def loadFile(program: String): Unit = {
    // create an analysis
    val analysis = createAnalysis(program)
    // remove the old visualisation if present
    if(currentVis.isDefined) {
      document.body.removeChild(currentVis.get)
    }
    // create a new visualisation
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    val webvis = createVisualisation(analysis, width, height)
    // load it in the main web page HTML
    document.body.appendChild(webvis)
    // update the state of the visualisation setup
    currentAnl = Some(analysis)
    currentVis = Some(webvis)
  }

  private def keyHandler: PartialFunction[String, Unit] = {
    case "e" | "E"        => runAnalysis(Timeout.none)
    case "n" | "N" | " "  => stepAnalysis() // Next step.
    case "r"              => runAnalysis(Timeout.start(Duration(5, SECONDS))) // Run 5 seconds.
    case "R"              => runAnalysis(Timeout.start(Duration(10, SECONDS))) // Run 10 seconds.
    case "s"              => stepMultiple(10)
    case "S"              => stepMultiple(25)
  }

  private def onClick() = stepAnalysis()

  private def stepAnalysis() = currentAnl.foreach { anl =>
    if(!anl.finished) {
      anl.step(Timeout.none)
    } else {
      println("The analysis has already terminated")
    }
  }

  private def runAnalysis(t: Timeout.T) = 
    currentAnl.foreach(_.analyzeWithTimeout(t))

  private def stepMultiple(count: Int) = {
    def loop(anl: Analysis, current: Int): Unit =
      if(current > 0) {
        anl.step(Timeout.none)
        //js.timers.setTimeout(100) { <- uncomment to see the analysis evolve more slowly
          loop(anl, current - 1)
        //}
      }
    if(currentAnl.isDefined) {
      loop(currentAnl.get, count)
    }
  }
}
