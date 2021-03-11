package maf.web.visualisation.softcontract

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
      extends IncrementalSchemeModFAnalysisCPLattice(program)
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
      ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with VisualisableIntraAnalysis {

      override def analyze(timeout: Timeout.T): Unit = {
        println(s"Analysing $cmp")
        super.analyze(timeout)
      }

      override def trigger(dep: Dependency): Unit = {
        println(s"$component triggers $dep")
        super.trigger(dep)
      }
    }
    try {
      println("Starting initial analysis.") // Will be logged to console.
      analyze(Timeout.start(Duration(5, MINUTES)))
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
  def createIncrementalVisualisation(text: String) = new WebVisualisationIncremental(newIncrementalReanalysis(text))

  def loadFile(text: String): Unit = {
    val visualisation = createIncrementalVisualisation(text)
    // parameters for the visualisation
    val body = document.body
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }
}


/*
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
*/
