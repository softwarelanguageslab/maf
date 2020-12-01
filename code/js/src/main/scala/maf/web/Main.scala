package maf.web

import org.scalajs.dom.html.TextArea
import org.scalajs.dom.raw.MouseEvent
import maf.core.Position._
import maf.core.Identity._
import maf.language.contracts.SCExpCompiler
import maf.language.scheme._
import maf.modular._
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.util.PrettyPrinter
import maf.util.benchmarks.Timeout
import maf.core.Position._
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
import org.scalajs.dom.{document, html, window}

import scala.scalajs.js
import maf.modular.scheme.modf.SimpleSchemeModFAnalysis
import org.scalajs.dom.raw.HashChangeEvent
import maf.language.contracts.ScExp
import maf.core.Identity
import maf.modular.scheme.modf.SimpleSchemeModFAnalysis

// Scala.js helpers

object FileInputElement {
  def apply(handler: String => Unit): html.Input = {
    val input = document.createElement("input").asInstanceOf[html.Input]
    input.setAttribute("type", "file")
    input.addEventListener(
      "change",
      (evtUpload: dom.Event) => {
        val file   = input.files.item(0)
        val reader = new dom.FileReader()
        reader.onload = (evtLoad: dom.Event) => handler(reader.result.asInstanceOf[String])
        reader.readAsText(file)
      },
      false
    )
    return input
  }
}

trait Component {
  def afterRender(): Unit = ()
  def render(): dom.Element
}

trait Router {
  private def path: String =
    if (window.top.location.hash.isEmpty) {
      "/"
    } else {
      window.top.location.hash.substring(1)
    }

  private def updateComponent(routes: Map[String, Component]): Unit = {
    val body = document.body
    body.innerHTML = ""
    val currentPath = path
    val component   = routes.get(currentPath)
    component match {
      case Some(c) => {
        body.appendChild(c.render())
        c.afterRender()
      }
      case None => println("404 not found")
    }
  }

  def routes(r: (String, Component)*): Unit = {
    val routesMap = r.toMap
    updateComponent(routesMap)
    window.addEventListener(
      "hashchange",
      (change: HashChangeEvent) => updateComponent(routesMap),
      false
    )
  }
}

object WebVisualisationPage extends Component {
  val input = FileInputElement(loadFile)

  def render(): dom.Element = {
    input
  }

  def standardAnalysis(program: SchemeExp) =
    new SimpleSchemeModFAnalysis(program) with SchemeModFNoSensitivity
    with SchemeConstantPropagationDomain with DependencyTracking[SchemeExp]
    with FIFOWorklistAlgorithm[SchemeExp] {
      //override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
      def key(cmp: Component) = expr(cmp).idn
      override def step(t: Timeout.T) = {
        val cmp = workList.head
        println(cmp)
        super.step(t)
      }
      override def intraAnalysis(cmp: SchemeModFComponent): IntraAnalysis with BigStepModFIntra =
        new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
    }

  def incrementalReanalysis(program: SchemeExp) =
    new IncrementalSchemeModFCPAnalysis(program) with DependencyTracking[SchemeExp] {
      override def intraAnalysis(cmp: SchemeModFComponent) =
        new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with DependencyTrackingIntra
      analyze(Timeout.start(Duration(5, MINUTES)))
      version = New
      optimisationFlag = true
      findUpdatedExpressions(program).flatMap(mapping).foreach(addToWorkList)
    }

  // Select the analysis to be used by the web visualisation.
  val analysis
      : SchemeExp => ModAnalysis[_] with SequentialWorklistAlgorithm[_] with DependencyTracking[_] =
    standardAnalysis

  def loadFile(text: String): Unit = {
    val program       = SchemeParser.parse(text)
    val visualisation = new WebVisualisation(analysis(program))
    // parameters for the visualisation
    val body   = document.body
    val width  = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }
}

object ContractVerificationPage extends Component {

  override def afterRender(): Unit = {
    import scalatags.JsDom.all._

    val results = document.querySelector("#resultcontent")
    val code    = document.querySelector("#code").asInstanceOf[TextArea]
    val safety  = document.querySelector(".safety")

    document
      .querySelector("#submitCode")
      .addEventListener(
        "click",
        (click: MouseEvent) => {
          val sc       = SCExpCompiler.read(code.value)
          val analysis = new ScTestAnalysisWeb(sc)
          def colorCode(code: ScExp): String = {
            val verifiedT = analysis
              .getVerificationResults(analysis.VerifiedTrue)
              .flatMap((t) => List(t._1.pos, t._2.pos))
            val verifiedF = analysis
              .getVerificationResults(analysis.VerifiedFalse)
              .flatMap((t) => List(t._1.pos, t._2.pos))
            val verifiedU = analysis
              .getVerificationResults(analysis.Top)
              .flatMap((t) => List(t._1.pos, t._2.pos))
            val blames = analysis.summary.blames.values.flatten

            val monitors = analysis.verificationResults.keys.map(_.pos).toList

            val printer = new PrettyPrinter() {
              override def print(s: String, idn: Identity = Identity.none) {
                println(verifiedT)
                println(idn.pos)
                if (verifiedT.contains(idn.pos)) {
                  super.print("<span class=\"highlight green\" >" + s + "</span>", idn)
                } else if (verifiedF.contains(idn.pos)) {
                  super.print("<span class=\"highlight red\" >" + s + "</span>", idn)
                } else if (verifiedU.contains(idn.pos)) {
                  super.print("<span class=\"highlight bold\" >" + s + "</span>", idn)
                } else if (monitors.contains(idn.pos)) {
                  super.print("<span class=\"highlight underline\" >" + s + "</span>", idn)
                } else if (blames.exists(_.blamedPosition.pos == idn.pos)) {
                  super.print("<span class=\"highlight blame\" >" + s + "</span>", idn)
                } else {
                  super.print(s, idn)
                }

              }
            }
            code.prettyPrint(printer)
            printer.render
          }

          analysis.analyze()
          safety.innerHTML = ""
          println(analysis.verificationResults)
          if (analysis.summary.blames.isEmpty) {
            results.innerHTML = colorCode(sc)
            safety.appendChild(span(`class` := "safe", "Verified as safe").render)
          } else {
            results.innerHTML = colorCode(sc)
            safety.appendChild(span(`class` := "unsafe", "Could not verify as safe").render)
          }

        },
        useCapture = false
      )
  }

  def render(): dom.Element = {
    import scalatags.JsDom.all._
    div(
      `class` := "main",
      h3("Soft-Contract Verification Demo"),
      div(
        `class` := "editor",
        textarea(id := "code"),
        br(),
        button(
          id := "submitCode",
          "Check"
        )
      ),
      div(
        `class` := "right",
        div(
          `class` := "results",
          pre(id := "resultcontent")
        ),
        div(
          `class` := "safety"
        )
      )
    ).render
  }
}

object Main extends Router {
  def main(args: Array[String]): Unit = routes(
    "/"          -> WebVisualisationPage,
    "/contracts" -> ContractVerificationPage
  )
}
