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
    a.updateAnalysis(timeout(), bench, true)
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a    = new IncrementalSchemeModFAnalysis(text)
    a.analyze(timeout())
    a.updateAnalysis(timeout(), bench)
  }

  val modConcbenchmarks: List[String]  = List("test/changes/cscheme/threads/pc.scm")
  val modFbenchmarks: List[String]     = List()
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach { bench =>
    // println(bench)
    //for (i <- 1 to 15) {
    // print(Timer.timeOnly({
    modconcAnalysis(bench, standardTimeout)
    //}) + " ")
    //}
  }
  modFbenchmarks.foreach { bench =>
    //println(bench)
    //for (i <- 1 to 15) {
    // print(Timer.timeOnly({
    modfAnalysis(bench, standardTimeout)
    // }) + " ")
    //}
  }
}
