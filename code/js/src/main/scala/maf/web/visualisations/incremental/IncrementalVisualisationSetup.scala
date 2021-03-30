package maf.web.visualisations.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.scheme._
import maf.modular._
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme.modf._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout
import maf.language.change.CodeVersion._
import maf.modular.incremental._

import scala.concurrent.duration._
import maf.web.visualisations._

import scala.concurrent.TimeoutException
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("incrementalVisualisationSetup")
object IncrementalVisualisationSetup extends VisualisationSetup {

  type Analysis = IncrementalAnalysis

  def createAnalysis(text: String) = {
    val program: SchemeExp = CSchemeParser.parse(text)
    new IncrementalAnalysis(program, IncrementalConfiguration.allOptimisations)
  }

  def createVisualisation(
      analysis: Analysis,
      width: Int,
      height: Int
    ) =
    (new WebVisualisationIncremental(analysis, width: Int, height: Int) with RetainAllIncremental with AddressVisualisationIncremental).node

  override def loadFile(program: String): Unit = {
    super.loadFile(program)
    //initAnalysis()
  }

  // Is called upon loading a file to perform extra steps.
  def initAnalysis(): Unit = {
    analysis.get.initAnalysis()
  }
}

class IncrementalAnalysis(program: SchemeExp, configuration: IncrementalConfiguration)
    extends IncrementalSchemeModFAssertionAnalysisCPLattice(program, configuration)
       with VisualisableIncrementalModAnalysis[SchemeExp] {

  var initialised: Boolean = false

  override def step(timeout: Timeout.T): Unit = {
    if (initialised)
      super.step(timeout)
    else println("Setup in progress, cannot step analysis.")
  }

  type Module = Option[SchemeLambdaExp]

  def module(cmp: Component) = cmp match {
    case Main                 => None
    case Call((lambda, _), _) => Some(lambda)
  }

  def moduleName(mdl: Module) = mdl.map(_.lambdaName).getOrElse("main")

  override def updateAddrInc(
      cmp: SchemeModFComponent,
      addr: Addr,
      nw: Value
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

  // We need to wait with doing this until the visualisation is created.
  def initAnalysis(): Unit = {
    try {
      println("Starting initial analysis.") // Will be logged to console.
      analyzeWithTimeout(Timeout.start(Duration(5, MINUTES)))
      if (finished)
        println("Finished initial analysis. Preparing for reanalysis.")
      else throw new TimeoutException("Initial analysis timed out.")
      version = New
      val affected = findUpdatedExpressions(program).flatMap(mapping)
      affected.foreach(addToWorkList)
      println(s"Directly affected components: ${affected.toList.mkString(", ")}")
      println("Preparation finished. Starting reanalysis.")
      initialised = true
    } catch {
      case t: Throwable =>
        System.err.println(t.getMessage) // Will display an error in the console.
        throw t
    }
  }
}
