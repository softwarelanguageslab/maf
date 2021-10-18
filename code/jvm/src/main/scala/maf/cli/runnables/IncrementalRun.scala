package maf.cli.runnables

import maf.language.CScheme._
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.scheme.modf._
import maf.modular.incremental._
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations._
import maf.modular.incremental.scheme.lattice._
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

object IncrementalRun extends App:

    // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
    // Useful when reducing a program when debugging the analysis.
    def interpretProgram(file: String): Unit =
        val prog = CSchemeParser.parseProgram(Reader.loadFile(file))
        val i = new SchemeInterpreter((_, _) => (), stack = true)
        print("*")
        i.run(prog, Timeout.start(Duration(3, MINUTES)), Old)
        print("*")
        i.run(prog, Timeout.start(Duration(3, MINUTES)), New)
        println("*")

    def modconcAnalysis(
        bench: String,
        config: IncrementalConfiguration,
        timeout: () => Timeout.T
      ): Unit =
        println(s"***** $bench *****")
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        val a = new IncrementalModConcAnalysisCPLattice(text, config) with IncrementalLogging[SchemeExp] {
          override def intraAnalysis(
              cmp: Component
            ) = new IntraAnalysis(cmp)
            with IncrementalSmallStepIntra
            with KCFAIntra
            with IncrementalGlobalStoreIntraAnalysis
            with IncrementalLoggingIntra {
            override def analyzeWithTimeout(timeout: Timeout.T): Unit =
                println(s"Analyzing $cmp")
                super.analyzeWithTimeout(timeout)
          }
        }
        a.analyzeWithTimeout(timeout())
        print(a.finalResult)
    //a.updateAnalysis(timeout())

    def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =
        def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
          new IncrementalSchemeModFAnalysisTypeLattice(text, configuration) with IncrementalLogging[SchemeExp] {
            override def focus(a: Addr): Boolean = a.toString.toLowerCase().nn.contains("ret")
            override val dependencyGraph: Boolean = true

            override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
              with IncrementalSchemeModFBigStepIntra
              with IncrementalGlobalStoreIntraAnalysis
              //  with AssertionModFIntra
              with IncrementalLoggingIntra
          }

        // Analysis from soundness tests.
        def base(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
          with StandardSchemeModFComponents
          with SchemeModFNoSensitivity
          with SchemeModFSemantics
          with LIFOWorklistAlgorithm[SchemeExp]
          with IncrementalSchemeModFBigStepSemantics
          with IncrementalSchemeTypeDomain // IncrementalSchemeConstantPropagationDomain
          with IncrementalGlobalStore[SchemeExp]
          with IncrementalLogging[SchemeExp] {
          override def focus(a: Addr): Boolean = !a.toString.contains("PrmAddr") // a.toString.contains("ret")
          override val dependencyGraph: Boolean = true
          var configuration: IncrementalConfiguration = wi_cy
          override def intraAnalysis(
              cmp: Component
            ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with IncrementalLoggingIntra
        }

        println(s"***** $bench *****")
        //  interpretProgram(bench)
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        println(text.prettyString())
        val a = base(text)
        a.logger.logU("BASE + INC")
        a.analyzeWithTimeout(timeout())
        //println(a.visited)
        a.updateAnalysis(timeout())
        //println(a.visited)
        Thread.sleep(1000)
        val b = base(text)
        b.version = New
        b.logger.logU("REAN")
        b.analyzeWithTimeout(timeout())
        // println(b.visited)
        println("Done")
    end modfAnalysis

    val modConcbenchmarks: List[String] = List()
    val modFbenchmarks: List[String] = List("test/changes/scheme/satCoarse.scm")
    val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(30, SECONDS))

    modConcbenchmarks.foreach(modconcAnalysis(_, ci_di_wi, standardTimeout))
    modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
