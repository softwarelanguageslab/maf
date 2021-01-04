package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.modular.GlobalStore
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

  def modfAnalysis(bench: String, timeout: () => Timeout.T) = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a    = new IncrementalSchemeModFCPAnalysisStoreOpt(text)
    a.analyze(timeout())
    a.store
    //a.updateAnalysis(timeout(), bench)
  }

  val modConcbenchmarks: List[String]  = List()
  val    modFbenchmarks: List[String]  = List("test/DEBUG1.scm", "test/DEBUG2.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach { bench => modconcAnalysis(bench, standardTimeout) }
  //modFbenchmarks.foreach { bench => modfAnalysis(bench, standardTimeout) }
  modFbenchmarks.map(modfAnalysis(_, standardTimeout)) match {
    case s1 :: s2 :: Nil =>
      assert(s1.keySet.map(_.hashCode()) == s2.keySet.map(_.hashCode()))
      s1.foreach({case (a, v) => {
        s2.find(_._1.hashCode() == a.hashCode()).get match {
          case (_, v2) if(v.toString != v2.toString) =>
            println(s"$a: $v")
            println(s"$a: $v2")
          case _ =>
        }}
      }
      )
  }
}
