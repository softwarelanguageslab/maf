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
import maf.language.scheme.primitives.SchemePrelude
import maf.language.CScheme.CSchemeLexicalAddresser
import maf.core._

object AdaptiveRun:

    def main(args: Array[String]): Unit = testTransform()

    def testConcrete() =
        val txt =
          """
          | (define x 2) 
          | x
          """.stripMargin
        val prg = CSchemeParser.parseProgram(txt)
        val int = new SchemeInterpreter(io = new FileIO(Map()))
        int.run(prg, Timeout.none)
        int.store.foreach {
          case (addr, value) if !addr._2.isInstanceOf[ConcreteValues.AddrInfo.PrmAddr] =>
            println(s"${addr} -> ${value}")
          case _ => ()
        }

    def testTransform(): Unit =
        val txt = Reader.loadFile("test/R5RS/various/strong-update.scm")
        val parsed = CSchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        val prg = CSchemeParser.undefine(transf)
        println(prg)

    def adaptiveAnalysisA(prg: SchemeExp, n: Int) =
      new SchemeModFLocal(prg) with SchemeConstantPropagationDomain with SchemeModFLocalCallSiteSensitivity(0) with FIFOWorklistAlgorithm[SchemeExp]:
          override def customPolicy(adr: Adr): AddrPolicy = AddrPolicy.Widened
          //override def debug(msg: => String): Unit = println(s"[DEBUG $i] $msg")
          var i = 0
          override def step(t: Timeout.T): Unit =
              i += 1
              val cmp = workList.head
              println(s"[$i] Analysing $cmp")
              super.step(t)
          def printStore(sto: Sto) =
            sto.content.view.toMap
              .foreach { case (a, (v, _)) => println(s"$a -> $v") }
          def printDelta(dlt: Dlt) =
            dlt.delta.view.toMap
              .foreach { case (a, (v, _)) => println(s"$a -> $v") }
          def printCmp(cmp: Cmp) =
              val (res, dlt) = results.getOrElse(cmp, (lattice.bottom, Delta.empty)).asInstanceOf[(Val, Dlt)]
              println()
              println(s"COMPONENT $cmp WHERE")
              printStore(cmp.sto)
              println(s"==> RESULTS: $res")
              println(s"==> DELTA (updated: ${dlt.updates.mkString("{", ",", "}")}):")
              printDelta(dlt)
              println()

    def testModFLocal(): Unit =
        val txt = Reader.loadFile("test/R5RS/gambit/peval.scm")
        val parsed = CSchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        val prg = CSchemeParser.undefine(transf)
        val anl = adaptiveAnalysisA(prg, 100)
        anl.analyze()

    def testModConc(): Unit =
        val txt = Reader.loadFile("test/concurrentScheme/threads/msort.scm")
        val prg = CSchemeParser.parseProgram(txt)
        val anl = SchemeAnalyses.modConcAnalysis(prg, 0)
        anl.analyzeWithTimeout(Timeout.start(Duration(60, SECONDS)))
        print(anl.store)

    def testAbstract(): Unit =
        val txt = Reader.loadFile("test/R5RS/various/my-test.scm")
        val prg = CSchemeParser.parseProgram(txt)
        val anl = new SimpleSchemeModFAnalysis(prg)
          with SchemeModFNoSensitivity
          with SchemeConstantPropagationDomain
          with FIFOWorklistAlgorithm[SchemeExp] {
          var step = 0
          override def step(timeout: Timeout.T): Unit =
              val cmp = workList.head
              step += 1
              println(s"[$step] Analysing ${view(cmp)}")
              super.step(timeout)
        }
        anl.analyze()
        //debugClosures(analysis)
        //println(anl.finished)
        debugResults(anl, false)

    def testAbstractAdaptive(): Unit =
        val txt = Reader.loadFile("test/R5RS/various/mceval.scm")
        val prg = CSchemeParser.parseProgram(txt)
        val anl = new AdaptiveModAnalysis(prg, rate = 1000)
          with AdaptiveSchemeModFSemantics
          with AdaptiveContextSensitivity
          with AdaptiveKCFA
          with SchemeConstantPropagationDomain
          with FIFOWorklistAlgorithm[SchemeExp] {
          // logging the analysis
          var step = 0
          override def step(timeout: Timeout.T): Unit =
              //val cmp = workList.head
              step += 1
              //println(s"[$step] Analysing ${view(cmp)}")
              super.step(timeout)
          override protected def debug(message: => String) = println(s"DEBUG: [$step] $message")
          override protected def warn(message: => String) = println(s"WARN: [$step] $message")
        }
        anl.analyze()
        //debugClosures(analysis)
        println("===================")
        println(s"Steps: ${anl.step}")
        println(s"Finished: ${anl.finished}")
        debugResults(anl, false)

    def debugResults(machine: BaseSchemeModFSemantics, printMore: Boolean = false): Unit =
      machine.store.foreach {
        case (ReturnAddr(cmp: machine.Component @unchecked, _), result) if cmp == machine.initialComponent || printMore =>
          println(s"$cmp => $result")
        case _ => ()
      }
