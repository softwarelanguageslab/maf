package maf.web

import maf.core.Identity
import maf.language.CScheme.CSchemeParser
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
import org.scalajs.dom._

import scala.scalajs.js
import maf.modular.scheme.modf.SimpleSchemeModFAnalysis

// Scala.js helpers

object FileInputElement {
  def apply(handler: String => Unit): html.Input = {
    val input = document.createElement("input").asInstanceOf[html.Input]
    input.setAttribute("type", "file")
    input.addEventListener(
      "change",
      (evtUpload: dom.Event) => {
        val file = input.files.item(0)
        val reader = new dom.FileReader()
        reader.onload = (evtLoad: dom.Event) => handler(reader.result.asInstanceOf[String])
        reader.readAsText(file)
      },
      false
    )
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

  def newStandardAnalysis(text: String) = {
    val program = SchemeParser.parse(text)
    new SimpleSchemeModFAnalysis(program)
      with SchemeModFNoSensitivity
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
  }

  class IncrementalAnalysis(program: SchemeExp)
      extends IncrementalSchemeModFAssertionAnalysisCPLattice(program)
         with VisualisableIncrementalModAnalysis[SchemeExp] {

    override def updateAddrInc(
        cmp: SchemeModFComponent,
        addr: Addr,
        nw: modularLatticeWrapper.modularLattice.L
      ): Boolean = {
      val old = provenance(addr)(cmp)
      println(s"$addr [$cmp]: $old => $nw")
      super.updateAddrInc(cmp, addr, nw)
    }

    override def deleteProvenance(cmp: SchemeModFComponent, addr: Addr): Unit = {
      val old = store.getOrElse(addr, lattice.bottom)
      super.deleteProvenance(cmp, addr)
      val nw = store.getOrElse(addr, lattice.bottom)
      println(s"$addr [$cmp]: $old _> $nw")
    }

    override def intraAnalysis(
        cmp: SchemeModFComponent
      ) = new IntraAnalysis(cmp)
      with IncrementalSchemeModFBigStepIntra
      with IncrementalGlobalStoreIntraAnalysis
      with AssertionModFIntra
      with VisualisableIntraAnalysis {

      override def analyzeWithTimeout(timeout: Timeout.T): Unit = {
        println(s"Analysing $cmp")
        super.analyzeWithTimeout(timeout)
      }

      override def trigger(dep: Dependency): Unit = {
        println(s"$component triggers $dep")
        super.trigger(dep)
      }
    }
    try {
      println("Starting initial analysis.") // Will be logged to console.
      analyzeWithTimeout(Timeout.start(Duration(5, MINUTES)))
      println("Finished initial analysis. Preparing for reanalysis.")
      version = New
      optimisationFlag = true
      val affected = findUpdatedExpressions(program).flatMap(mapping)
      affected.foreach(addToWorkList)
      println(s"Directly affected components: ${affected.toList.mkString(", ")}")
      println("Preparation finished. Starting reanalysis.")
    } catch {
      case t: Throwable =>
        System.err.println(t.getMessage) // Will display an error in the console.
        throw t
    }
  }

  def newIncrementalReanalysis(text: String): IncrementalSchemeModFAnalysisCPLattice with VisualisableIncrementalModAnalysis[SchemeExp] = {
    val program: SchemeExp = CSchemeParser.parse(text)
    new IncrementalAnalysis(program)
  }

  def createVisualisation(text: String) = new WebVisualisation(newStandardAnalysis(text))

  def createIncrementalVisualisation(
      text: String
    ) = new WebVisualisationIncremental(newIncrementalReanalysis(text)) with RetainAllIncremental with AddressVisualisationIncremental

  def loadFile(text: String): Unit = {
    val visualisation = createIncrementalVisualisation(text)
    // parameters for the visualisation
    val body = document.body
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }
}
