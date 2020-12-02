package maf.web

import maf.core.Identity
import maf.language.scheme._
import maf.modular._
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.util.benchmarks.Timeout
import maf.language.change.CodeVersion._

import scala.concurrent.duration._

// Scala.js-related imports
import org.scalajs.dom
import org.scalajs.dom.{document, html}

import scala.scalajs.js
import maf.modular.scheme.modf.SimpleSchemeModFAnalysis

// Scala.js helpers

object FileInputElement {
  def apply(handler: String => Unit): html.Input = {
    val input = document.createElement("input").asInstanceOf[html.Input]
    input.setAttribute("type","file")
    input.addEventListener("change", (evtUpload: dom.Event) => {
      val file = input.files.item(0)
      val reader = new dom.FileReader()
      reader.onload = (evtLoad: dom.Event) => handler(reader.result.asInstanceOf[String])
      reader.readAsText(file)
    }, false)
    return input
  }
}

object Main {
  val input = FileInputElement(loadFile)

  def main(args: Array[String]): Unit = setupUI()

  def setupUI() = {
    val body = document.body
    body.appendChild(input)
  }

  def standardAnalysis(program: SchemeExp): SimpleSchemeModFAnalysis with SchemeModFNoSensitivity with SchemeConstantPropagationDomain with DependencyTracking[SchemeExp] with FIFOWorklistAlgorithm[SchemeExp] = new SimpleSchemeModFAnalysis(program) with SchemeModFNoSensitivity
    with SchemeConstantPropagationDomain
    with DependencyTracking[SchemeExp]
    with FIFOWorklistAlgorithm[SchemeExp] {
    //override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
    def key(cmp: Component): Identity = expr(cmp).idn
    override def step(t: Timeout.T) = {
      val cmp = workList.head
      println(cmp)
      super.step(t)
    }
    override def intraAnalysis(cmp: SchemeModFComponent): IntraAnalysis with BigStepModFIntra =
      new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
  }

  def incrementalReanalysis(program: SchemeExp): IncrementalSchemeModFCPAnalysisStoreOpt with DependencyTracking[SchemeExp] = new IncrementalSchemeModFCPAnalysisStoreOpt(program) with DependencyTracking[SchemeExp] {
    override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalReturnValueIntraAnalysis with DependencyTrackingIntra
    println("Starting initial analysis.") // Will be logged to console.
    try {
      analyze(Timeout.start(Duration(5, MINUTES)))
    } catch {
      case t: Throwable => System.err.println(t.getMessage) // Will display an error in the console.
        throw t
    }
    println("Finished initial analysis. Preparing for reanalysis.")
    version = New
    optimisationFlag = true
    findUpdatedExpressions(program).flatMap(mapping).foreach(addToWorkList)
    println("Preparation finished. Starting reanalysis.")
  }

  // Select the analysis to be used by the web visualisation.
  val analysis: SchemeExp => ModAnalysis[_] with SequentialWorklistAlgorithm[_] with DependencyTracking[_] = incrementalReanalysis

  def loadFile(text: String): Unit = {
    val program = SchemeParser.parse(text)
    val visualisation = new WebVisualisation(analysis(program))
    // parameters for the visualisation
    val body = document.body
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }
}