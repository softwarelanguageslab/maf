package maf.web.visualisations

import org.scalajs.dom._
import maf.web._
import scala.scalajs.js.annotation._
import scala.scalajs.js

trait SimpleVisualisationSetup {

  @JSExport
  def init() = {
    val input = FileInputElement(loadFile)
    document.body.appendChild(input)
  }

  def loadFile(text: String): Unit = {
    val visualisation = createVisualisation(text)
    // parameters for the visualisation
    val body = document.body
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }

  def createVisualisation(program: String): WebVisualisation
}
