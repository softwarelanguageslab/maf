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
import org.scalajs.dom.raw.HTMLElement

object ExamplePrograms:
    val factorial: String =
        """|(define (factorial n)
      | (if (= n 0)
      |      1
      |      (* n (factorial (- n 1)))))
      |(factorial 5)""".stripMargin

trait VisualisationSetup:

    type Analysis <: ModAnalysis[_] with SequentialWorklistAlgorithm[_]

    // the current state of the visualisation setup
    var current: Option[(Analysis, dom.Node)] = None
    def analysis: Option[Analysis] = current.map(_._1)
    def webvis: Option[dom.Node] = current.map(_._2)
    var nextButton: html.Element = _
    var input: EditText = _
    var storeVisualisation: HTMLElement = _

    // create some visualisation for the given program (with given dimensions)
    def createAnalysis(program: String): Analysis
    def createVisualisation(
        analysis: Analysis,
        width: Int,
        height: Int
      ): dom.Node

    def setupStoreVisualisation(container: HTMLElement): Unit

    @JSExport
    def setup() =
        // add element to provide a program to the analysis
        input = EditText(loadFile)
        input.setFile(ExamplePrograms.factorial)
        document.body.appendChild(input.render())

        // Add some buttons
        nextButton = Button("Click 'Start Analysis' to start.")(onClick())
        nextButton.classList.add("btn")
        nextButton.classList.add("hidden")
        input.appendChild(nextButton)

        // Add the container that holds both the graph visualisation
        // as well as the store visualisation
        val container = document.createElement("div")
        container.setAttribute("id", "visualisationContainer")
        document.body.appendChild(container)

        // Add the container for holding the store visualisation
        storeVisualisation = document.createElement("div").asInstanceOf[HTMLElement]
        storeVisualisation.setAttribute("id", "storeVisualisation")
        container.appendChild(storeVisualisation)

        // Add the visualisation div
        val div = document.createElement("div")
        div.classList.add("visualisation")
        container.appendChild(div)

        // input handling
        val body = d3.select(document.body)

    protected def loadFile(program: String): Unit =
        nextButton.innerText = "Next"
        nextButton.classList.remove("hidden")
        nextButton.classList.remove("disabled")
        // create an analysis
        val analysis = createAnalysis(program)
        // remove the old visualisation if present
        this.webvis.foreach {
            document.querySelector(".visualisation").removeChild(_)
        }
        // remove the old store visualisation
        storeVisualisation.innerHTML = ""
        // create a new visualisation
        val width = document.querySelector(".visualisation").asInstanceOf[HTMLElement].offsetWidth.asInstanceOf[Int]
        val height = document.querySelector(".visualisation").asInstanceOf[HTMLElement].offsetHeight.asInstanceOf[Int]
        val webvis = createVisualisation(analysis, width, height)
        setupStoreVisualisation(storeVisualisation)
        // load it in the main web page HTML
        document.querySelector(".visualisation").appendChild(webvis)
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

    protected def onClick() =
        this.analysis.foreach(stepAnalysis)

    private def stepAnalysis(anl: Analysis) =
        if !anl.finished then anl.step(Timeout.none)
        else
            nextButton.innerText = "Analysis is finished"
            nextButton.classList.add("disabled")
            input.reset()
            println("The analysis has already terminated")

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
