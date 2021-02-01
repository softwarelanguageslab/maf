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
import maf.language.sexp.SExpParser
import scala.util.parsing.input.CharSequenceReader
import maf.language.scheme.SchemeInterpreter
import maf.language.scheme.FileIO

object AdaptiveRun {

  def main(args: Array[String]): Unit = testAbstract()

  def testConcrete() {
    val txt = """
    (define x 2) x
    """
    val prg = CSchemeParser.parse(txt)
    val int = new SchemeInterpreter(io = new FileIO(Map()))
    int.run(prg, Timeout.none)
    int.store.foreach {
      case (addr, value) if !addr._2.isInstanceOf[maf.language.scheme.SchemeInterpreter.AddrInfo.PrmAddr] =>
        println(s"${addr} -> ${value}")
      case _ => ()
    }
  }

  def testAbstract(): Unit = {
    val txt = Reader.loadFile("test/R5RS/various/mceval.scm")
    val prg = CSchemeParser.parse(txt)
    val anl = new AdaptiveModAnalysis(prg)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp] {
      lazy val budget = 1000
      override def step(timeout: Timeout.T): Unit = {
        //val cmp = workList.head
        //println(s"Analysing ${view(cmp)}")
        super.step(timeout)
      }
    }
    anl.analyze(Timeout.start(Duration(300, SECONDS)))
    //debugClosures(analysis)
    debugResults(anl, false)
  }

  def debugResults(machine: SchemeModFSemantics, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ReturnAddr(cmp: machine.Component, _), result) if cmp == machine.initialComponent || printMore =>
        println(s"${machine.view(cmp)} => $result")
      case _ => ()
    }
}
