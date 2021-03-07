package maf.web.visualisations

import org.scalajs._
import org.scalajs.dom._
import maf.web.utils._
import scala.scalajs.js.annotation._
import scala.scalajs.js

trait VisualisationSetup {

  var currentVis: Option[dom.Node] = None

  @JSExport
  def init() = {
    val input = FileInputElement(loadFile)
    document.body.appendChild(input)
  }

  // create some visualisation for the given program (with given dimensions)
  def create(program: String, width: Int, height: Int): dom.Node

  private def loadFile(program: String): Unit = {
    // remove the old visualisation if present
    if(currentVis.isDefined) {
      document.body.removeChild(currentVis.get)
    }
    // create a new visualisation
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    val webvis = create(program, width, height)
    // load it in the main web page HTML
    currentVis = Some(webvis)
    document.body.appendChild(webvis)
  }
}
