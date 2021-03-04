package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.ReturnAddr
import maf.modular.adaptive.AdaptiveModAnalysis
import maf.modular.adaptive.scheme._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._
import maf.cli.experiments.SchemeAnalyses
import maf.language.scheme.interpreter._

object AdaptiveRun {

  def main(args: Array[String]): Unit = testAbstract()

  def testConcrete() = {
    val txt = """
    (define x 2) x
    """
    val prg = CSchemeParser.parse(txt)
    val int = new SchemeInterpreter(io = new FileIO(Map()))
    int.run(prg, Timeout.none)
    int.store.foreach {
      case (addr, value) if !addr._2.isInstanceOf[ConcreteValues.AddrInfo.PrmAddr] =>
        println(s"${addr} -> ${value}")
      case _ => ()
    }
  }

  def testModConc(): Unit = {
    val txt = Reader.loadFile("test/concurrentScheme/threads/msort.scm")
    val prg = CSchemeParser.parse(txt)
    val anl = SchemeAnalyses.modConcAnalysis(prg, 0)
    anl.analyzeWithTimeout(Timeout.start(Duration(60, SECONDS)))
    print(anl.store)
  }

  def testAbstract(): Unit = {
    val txt = Reader.loadFile("test/R5RS/various/mceval.scm")
    val prg = CSchemeParser.parse(txt)
    val anl = new AdaptiveModAnalysis(prg)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp] {
      lazy val budget = 100
      var step = 0
      override def step(timeout: Timeout.T): Unit = {
        val cmp = workList.head
        println(s"[$step] Analysing ${view(cmp)}")
        step += 1
        super.step(timeout)
      }
    }
    anl.analyze()
    //debugClosures(analysis)
    println(anl.finished)
    debugResults(anl, false)
  }

  def debugResults(machine: SchemeModFSemantics, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ReturnAddr(cmp: machine.Component, _), result) if cmp == machine.initialComponent || printMore =>
        println(s"${machine.view(cmp)} => $result")
      case _ => ()
    }
}
