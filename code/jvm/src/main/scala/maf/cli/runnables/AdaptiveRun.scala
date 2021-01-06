package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.ReturnAddr
import maf.modular.adaptive.AdaptiveModAnalysis
import maf.modular.adaptive.scheme._
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.worklist._
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

object AdaptiveRun {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/R5RS/mceval.scm")
    val prg = CSchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp] {
      val budget = 500
      override def step(timeout: Timeout.T): Unit = {
        val cmp = workList.head
        println(view(cmp))
        super.step(timeout)
      }
    }
    analysis.analyze(Timeout.start(Duration(300, SECONDS)))
    //debugClosures(analysis)
    debugResults(analysis, false)
  }

  def debugResults(machine: AdaptiveSchemeModFSemantics, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ReturnAddr(cmp: machine.Component, _), result) if cmp == machine.initialComponent || printMore =>
        println(s"${machine.view(cmp)} => $result")
      case _ => ()
    }
}
