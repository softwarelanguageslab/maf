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
    val a    = new IncrementalModConcCPAnalysisStoreOpt(text)
    a.analyze(timeout())
    //a.updateAnalysis(timeout(), bench, true)
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a    = new IncrementalSchemeModFCPAnalysisStoreOpt(text)
    a.analyze(timeout())
    a.updateAnalysis(timeout(), bench)
  }

  val modConcbenchmarks: List[String]  = List()
  val    modFbenchmarks: List[String]  = List("test/DEBUG.scm", "test/DEBUG2.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach { bench => modconcAnalysis(bench, standardTimeout) }
  modFbenchmarks.foreach { bench => modfAnalysis(bench, standardTimeout) }
}
