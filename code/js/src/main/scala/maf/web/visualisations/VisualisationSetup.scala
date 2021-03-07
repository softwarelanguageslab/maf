package maf.web.visualisations

import maf.modular._
import maf.modular.worklist._

import org.scalajs._
import org.scalajs.dom._
import maf.web.utils._
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
    val input = FileInputElement(loadFile)
    document.body.appendChild(input)
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
}
