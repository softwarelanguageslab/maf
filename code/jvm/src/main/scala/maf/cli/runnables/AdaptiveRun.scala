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
import maf.modular.scheme.modflocal._

import scala.concurrent.duration._
import scala.language.reflectiveCalls

import maf.cli.experiments.SchemeAnalyses
import maf.language.scheme.interpreter._
import maf.language.scheme.SchemeMutableVarBoxer

object AdaptiveRun {

  def main(args: Array[String]): Unit = testTransform()

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

  def testTransform(): Unit = {
    val prg = """
      (lambda (x y)
        (set! x y)
        x)
    """
    val parsed = CSchemeParser.parse(prg)
    val transf = SchemeMutableVarBoxer.transform(parsed, Set("+", "-"))
    println(transf)
  }

  def testModFLocal(): Unit = {
    val txt = Reader.loadFile("test/R5RS/various/grid.scm")
    val prg = CSchemeParser.parse(txt)
    val anl = new SchemeModFLocal(prg) with SchemeConstantPropagationDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]
    def printStore(sto: anl.Sto) =
      sto.content.view
        .filterKeys(!_.isInstanceOf[PrmAddr])
        .filterKeys(!_.isInstanceOf[anl.EnvAddr])
        .toMap
        .foreach { case (a, s) =>
          println(s"$a -> ${sto.value(s).asInstanceOf[anl.V].vlu}")
        }
    anl.analyzeWithTimeoutInSeconds(10)
    anl.visited
      .collect { case cll: anl.CallComponent => cll }
      .foreach { case cmp @ anl.CallComponent(lam, _, sto) =>
        println()
        println(s"COMPONENT ${lam.lambdaName} WHERE")
        printStore(sto)
        println(s"==> ${anl.results(cmp)}")
        println()
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
    val txt = Reader.loadFile("test/R5RS/various/my-test.scm")
    val prg = CSchemeParser.parse(txt)
    val anl = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeConstantPropagationDomain with FIFOWorklistAlgorithm[SchemeExp] {
      var step = 0
      override def step(timeout: Timeout.T): Unit = {
        val cmp = workList.head
        step += 1
        println(s"[$step] Analysing ${view(cmp)}")
        super.step(timeout)
      }
    }
    anl.analyze()
    //debugClosures(analysis)
    //println(anl.finished)
    debugResults(anl, false)
  }

  def testAbstractAdaptive(): Unit = {
    val txt = Reader.loadFile("test/R5RS/various/mceval.scm")
    val prg = CSchemeParser.parse(txt)
    val anl = new AdaptiveModAnalysis(prg, rate = 1000)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp] {
      // logging the analysis
      var step = 0
      override def step(timeout: Timeout.T): Unit = {
        //val cmp = workList.head
        step += 1
        //println(s"[$step] Analysing ${view(cmp)}")
        super.step(timeout)
      }
      override protected def debug(message: => String) = println(s"DEBUG: [$step] $message")
      override protected def warn(message: => String) = println(s"WARN: [$step] $message")
    }
    anl.analyze()
    //debugClosures(analysis)
    println("===================")
    println(s"Steps: ${anl.step}")
    println(s"Finished: ${anl.finished}")
    debugResults(anl, false)
  }

  def debugResults(machine: BaseSchemeModFSemantics, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ReturnAddr(cmp: machine.Component @unchecked, _), result) if cmp == machine.initialComponent || printMore =>
        println(s"$cmp => $result")
      case _ => ()
    }
}
