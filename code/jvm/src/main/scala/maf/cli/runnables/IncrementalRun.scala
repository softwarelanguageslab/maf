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
    a.updateAnalysis(timeout(), bench)
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T) = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a    = new IncrementalSchemeModFCPAnalysisStoreOpt(text)
    a.analyze(timeout())
    a.updateAnalysis(timeout(), bench)
    a.cachedSpawns.filter(_._1.toString.contains("s!")).head._2.foreach(println)
  }

  val modConcbenchmarks: List[String]  = List("test/changes/cscheme/threads/crypt2.scm")
  val modFbenchmarks: List[String]     = List() //List("test/DEBUG3.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach(modconcAnalysis(_, standardTimeout))
  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
}
