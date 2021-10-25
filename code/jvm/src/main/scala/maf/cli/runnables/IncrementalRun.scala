package maf.cli.runnables

import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.scheme.modf.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.Timeout
import maf.util.graph.DotGraph
import maf.util.graph.DotGraph.*

import scala.concurrent.duration.*

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
          new IncrementalSchemeModFAnalysisTypeLattice(text, configuration)
            with IncrementalLogging[SchemeExp]
            with IncrementalDataFlowVisualisation[SchemeExp] {
            override def focus(a: Addr): Boolean = a.toString.toLowerCase().nn.contains("ret")

            override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
              with IncrementalSchemeModFBigStepIntra
              with IncrementalGlobalStoreIntraAnalysis
              //  with AssertionModFIntra
              with IncrementalLoggingIntra
              with IncrementalVisualIntra
          }

        // Analysis from soundness tests.
        def base(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
          with StandardSchemeModFComponents
          with SchemeModFNoSensitivity
          with SchemeModFSemanticsM
          with LIFOWorklistAlgorithm[SchemeExp]
          with IncrementalSchemeModFBigStepSemantics
          with IncrementalSchemeTypeDomain // IncrementalSchemeConstantPropagationDomain
          with IncrementalGlobalStore[SchemeExp]
          with IncrementalLogging[SchemeExp]
          with IncrementalDataFlowVisualisation[SchemeExp] {
          override def focus(a: Addr): Boolean =
            !a.toString.contains("PrmAddr") && (a.toString.contains("ret") || a.toString.contains("x2") || a.toString.contains("__"))
          var configuration: IncrementalConfiguration = wi_cy
          override def intraAnalysis(
              cmp: Component
            ) = new IntraAnalysis(cmp)
            with IncrementalSchemeModFBigStepIntra
            with IncrementalGlobalStoreIntraAnalysis
            with IncrementalLoggingIntra
            with IncrementalVisualIntra
        }

        println(s"***** $bench *****")
        interpretProgram(bench)
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        println(text.prettyString())
        val a = base(text)
        a.logger.logU("BASE + INC")
        a.analyzeWithTimeout(timeout())
        a.flowInformationToDotGraph("logs/flowsA1.dot")
        a.updateAnalysis(timeout())
        a.flowInformationToDotGraph("logs/flowsA2.dot")
        Thread.sleep(1000)
        val b = base(text)
        b.version = New
        b.logger.logU("REAN")
        b.analyzeWithTimeout(timeout())
        b.flowInformationToDotGraph("logs/flowsB.dot")
        println("Done")
    end modfAnalysis

    val modConcbenchmarks: List[String] = List()
    val modFbenchmarks: List[String] = List("test/DEBUG2.scm")
    val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(30, SECONDS))

    modConcbenchmarks.foreach(modconcAnalysis(_, ci_di_wi, standardTimeout))
    modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
    println("Creating graphs")
    createPNG("logs/flowsA1.dot", true)
    createPNG("logs/flowsA2.dot", true)
    createPNG("logs/flowsB.dot", true)
    println("Done")
