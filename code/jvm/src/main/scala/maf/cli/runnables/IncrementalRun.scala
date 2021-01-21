package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalLogging
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme.modf.SchemeModFComponent
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

object IncrementalRun extends App {

  def modconcAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new IncrementalModConcAnalysisCPLattice(text)
    a.analyze(timeout())
    a.updateAnalysis(timeout())
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    def newAnalysis(text: SchemeExp) = new IncrementalSchemeModFAssertionAnalysisCPLattice(text) with IncrementalLogging[SchemeExp] {
      override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
        with IncrementalSchemeModFBigStepIntra
        with IncrementalGlobalStoreIntraAnalysis
        with AssertionModFIntra
        with IncrementalLoggingIntra
    }

    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = newAnalysis(text)
    a.analyze(timeout())
    a.printAssertions()
    val aC = a.deepCopy()
    a.updateAnalysis(timeout(), false)
    a.printAssertions()
    aC.updateAnalysis(timeout())
    aC.printAssertions()
    val b = newAnalysis(text)
    b.version = New
    b.analyze(timeout())
    b.printAssertions()
  }

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/DEBUG3.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach(modconcAnalysis(_, standardTimeout))
  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
}
