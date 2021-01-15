package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.modular.incremental.scheme.SchemeAnalyses._
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
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new IncrementalSchemeModFAssertionAnalysisCPLattice(text)
    a.analyze(timeout())
    a.updateAnalysis(timeout())
    a.printAssertions()
  }

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changes/scheme/assertions/fold-fun-list.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach(modconcAnalysis(_, standardTimeout))
  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
}
