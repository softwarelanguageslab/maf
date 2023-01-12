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
import maf.modular.scheme.aam.*

import scala.concurrent.duration._
import scala.language.reflectiveCalls

import maf.cli.experiments.SchemeAnalyses
import maf.language.scheme.interpreter._
import maf.language.scheme.SchemeMutableVarBoxer
import maf.language.scheme.primitives.SchemePrelude
import maf.language.CScheme.CSchemeLexicalAddresser
import maf.core._

object AdaptiveRun:

    def main(args: Array[String]): Unit = print(runAAM(parse(txt)))

    val txt = 
            """
            | (define (foo x)
            |    (+ x 1))
            | (define x 5)
            | (foo 1)
            | x
            """.stripMargin

    def testConcrete() =
        val prg = CSchemeParser.parseProgram(txt)
        val int = new SchemeInterpreter(io = new FileIO(Map()))
        int.run(prg, Timeout.none)
        int.store.foreach {
            case (addr, value) if !addr._2.isInstanceOf[ConcreteValues.AddrInfo.PrmAddr] =>
                println(s"${addr} -> ${value}")
            case _ => ()
        }

    def testTransform(): Unit =
        val txt = Reader.loadFile("test/DEBUG2.scm")
        val parsed = CSchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        val prg = CSchemeParser.undefine(transf)
        println(prg.prettyString(2))

    def testModFLocal(): Unit =
        val txt = Reader.loadFile("test/R5RS/gambit/peval.scm")
        val parsed = CSchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        val prg = CSchemeParser.undefine(transf)
        val anl = SchemeAnalyses.modflocalAnalysis(prg, 100)
        anl.analyze()

    def parse(txt: String): SchemeExp =
        val parsed = CSchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        CSchemeParser.undefine(transf)

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

    def runDSS(prg: SchemeExp) =
        val anl = SchemeAnalyses.modflocalAnalysis(prg, 0)
        anl.analyze()
        val res = anl.results(anl.MainComponent).asInstanceOf[Set[(anl.Val, anl.Dlt, Set[anl.Adr])]]
        val vlu = Lattice[anl.Val].join(res.map(_._1))
        vlu 

    def runAAM(prg: SchemeExp) = 
        val aam = new SchemeAAMAnalysis(prg, 0)
        aam.analyze()
        aam.finalValue