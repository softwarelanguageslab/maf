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

trait VisualisationSetup:

    type Analysis <: ModAnalysis[_] with SequentialWorklistAlgorithm[_]

    // the current state of the visualisation setup
    var current: Option[(Analysis, dom.Node)] = None
    def analysis: Option[Analysis] = current.map(_._1)
    def webvis: Option[dom.Node] = current.map(_._2)

    // create some visualisation for the given program (with given dimensions)
    def createAnalysis(program: String): Analysis
    def createVisualisation(
        analysis: Analysis,
        width: Int,
        height: Int
      ): dom.Node

    @JSExport
    def setup() =
        // add an element to select a file
        val input = FileInputElement(loadFile)
        document.body.appendChild(input)
        // input handling
        val body = d3.select(document.body)
        body.on("keypress", () => keyHandler(d3.event.key.asInstanceOf[String]))
        body.on("click", () => onClick())

    protected def loadFile(program: String): Unit =
        // create an analysis
        val analysis = createAnalysis(program)
        // remove the old visualisation if present
        this.webvis.foreach {
          document.body.removeChild(_)
        }
        // create a new visualisation
        val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
        val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
        val webvis = createVisualisation(analysis, width, height)
        // load it in the main web page HTML
        document.body.appendChild(webvis)
        // update the state of the visualisation setup
        current = Some((analysis, webvis))

    protected def keyHandler(key: String): Unit =
      if analysis.isDefined then analysisCommandHandler(analysis.get).lift(key)

    protected def analysisCommandHandler(anl: Analysis): PartialFunction[String, Unit] =
        case "n" | "N" | " " => stepAnalysis(anl)
        case "e" | "E"       => stepUntil(anl)
        case "r"             => stepUntil(anl, timeout = Timeout.start(Duration(5, SECONDS)))
        case "R"             => stepUntil(anl, timeout = Timeout.start(Duration(10, SECONDS)))
        case "s"             => stepUntil(anl, stepLimit = Some(10))
        case "S"             => stepUntil(anl, stepLimit = Some(25))

    protected def onClick() = this.analysis.foreach(stepAnalysis)

    private def stepAnalysis(anl: Analysis) =
      if !anl.finished then anl.step(Timeout.none)
      else println("The analysis has already terminated")

    private def stepUntil(
        anl: Analysis,
        timeout: Timeout.T = Timeout.none,
        stepLimit: Option[Int] = None
      ): Unit =
      if !anl.finished && !timeout.reached && stepLimit.map(_ > 0).getOrElse(true) then
          anl.step(timeout)
          js.timers.setTimeout(0) { // <- gives JS time to update between steps
            stepUntil(anl, timeout, stepLimit.map(_ - 1))
          }
