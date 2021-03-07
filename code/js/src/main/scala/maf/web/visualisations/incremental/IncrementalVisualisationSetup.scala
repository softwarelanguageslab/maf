package maf.web.visualisations.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.scheme._
import maf.modular._
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme.modf._
import maf.util.benchmarks.Timeout
import maf.language.change.CodeVersion._
import maf.modular.incremental._
import scala.concurrent.duration._
import maf.web.visualisations._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("incrementalVisualisationSetup")
object IncrementalVisualisationSetup extends VisualisationSetup {

  def newIncrementalReanalysis(text: String): IncrementalSchemeModFAnalysisCPLattice with VisualisableIncrementalModAnalysis[SchemeExp] = {
    val program: SchemeExp = CSchemeParser.parse(text)
    new IncrementalAnalysis(program, IncrementalConfiguration.allOptimisations)
  }

  def create(text: String, width: Int, height: Int) =
    (new WebVisualisationIncremental(newIncrementalReanalysis(text), width: Int, height: Int) with RetainAllIncremental with AddressVisualisationIncremental).node
}

class IncrementalAnalysis(program: SchemeExp, configuration: IncrementalConfiguration)
    extends IncrementalSchemeModFAssertionAnalysisCPLattice(program, configuration)
       with VisualisableIncrementalModAnalysis[SchemeExp] {

  override def updateAddrInc(
      cmp: SchemeModFComponent,
      addr: Addr,
      nw: Value,
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
