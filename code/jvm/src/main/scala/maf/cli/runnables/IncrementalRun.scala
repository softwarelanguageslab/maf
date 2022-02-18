package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.incremental.SplitPerformance
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.{GlobalStore, ModAnalysis}
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.scheme.modf.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.ProgramVersionExtracter.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.{PrmAddr, SchemeTypeDomain}
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.{Reader, Writer}
import maf.util.Writer.Writer
import maf.util.benchmarks.{Timeout, Timer}
import maf.util.graph.DotGraph
import maf.util.graph.DotGraph.*

import scala.concurrent.duration.*

object IncrementalRun extends App:

    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
      new IncrementalSchemeModFAnalysisTypeLattice(text, configuration)
        with IncrementalLogging[SchemeExp]
        //with IncrementalDataFlowVisualisation[SchemeExp]
        {
        override def focus(a: Addr): Boolean = a.toString.toLowerCase().nn.contains("ret")

        override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          //  with AssertionModFIntra
          with IncrementalLoggingIntra
        //with IncrementalVisualIntra
      }

    // Performance benchmarks
    def perfAnalysis(e: SchemeExp, config: IncrementalConfiguration) = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
      with SplitPerformance[SchemeExp]
      with IncrementalLogging[SchemeExp] {
      mode = Mode.Summary
      override def intraAnalysis(cmp: Component) =
        new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          with SplitPerformanceIntra
          with IncrementalLoggingIntra
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
      //with IncrementalDataFlowVisualisation[SchemeExp]
      {
      override def focus(a: Addr): Boolean = false // a.toString.contains("VarAddr(n")
      var configuration: IncrementalConfiguration = ci
      mode = Mode.Fine
      override def intraAnalysis(
          cmp: Component
        ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with IncrementalLoggingIntra
      //with IncrementalVisualIntra
    }

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

    def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =
      try {
        println(s"***** $bench *****")
        //interpretProgram(bench)
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        //println(text.prettyString())
        val a = perfAnalysis(text, noOptimisations)
        a.logger.logU(bench)
        a.logger.logU("BASE")
        //println(a.configString())
        //a.version = New
        val timeI = Timer.timeOnly {
          a.analyzeWithTimeout(timeout())
        }
        if a.finished then println(s"Initial analysis took ${timeI / 1000000} ms.")
        else
            println(s"Initial analysis timed out after ${timeI / 1000000} ms.")
            return
        //a.visited.foreach(println)
        //println(a.store.filterNot(_._1.isInstanceOf[PrmAddr]))
        //a.configuration = noOptimisations
        // a.flowInformationToDotGraph("logs/flowsA1.dot")
        val a2 = perfAnalysis(text, ci_di_wi)
        a2.logger.logU("INC")
        a2.analyzeWithTimeout(timeout())
        a2.configuration = di_wi
        val timeU = Timer.timeOnly {
          a2.updateAnalysis(timeout())
        }
        if a.finished then println(s"Updating analysis took ${timeU / 1000000} ms.")
        else println(s"Updating analysis timed out after ${timeU / 1000000} ms.")
        // a.flowInformationToDotGraph("logs/flowsA2.dot")

        Thread.sleep(1000)
        val b = perfAnalysis(text, noOptimisations)
        b.version = New
        b.logger.logU("REAN")
        val timeR = Timer.timeOnly {
          b.analyzeWithTimeout(timeout())
        }
        if b.finished then println(s"Full reanalysis took ${timeR / 1000000} ms.")
        else println(s"Full reanalysis timed out after ${timeR / 1000000} ms.")
        // b.flowInformationToDotGraph("logs/flowsB.dot")
        println("Done")
        //println(a.program.asInstanceOf[SchemeExp].prettyString())
        //println(a.store.filterNot(_._1.isInstanceOf[PrmAddr]))
      } catch {
        case e: Exception =>
          e.printStackTrace(System.out)
          val w = Writer.open("benchOutput/incremental/errors.txt")
          Writer.writeln(w, bench)
          Writer.writeln(w, e.getStackTrace().toString)
          Writer.writeln(w, "")
          Writer.close(w)
      }
    end modfAnalysis

    val modFbenchmarks: List[String] = List(
      // "test/changes/scheme/generated/R5RS_gambit_sboyer-1.scm",
      "test/changes/scheme/generated/R5RS_icp_icp_1c_ontleed-4.scm"
      //"test/changes/scheme/reinforcingcycles/cycleCreation.scm"
      //"test/R5RS/gambit/nboyer.scm",
      //"test/changes/scheme/generated/R5RS_gambit_nboyer-5.scm"
    )
    val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(60, MINUTES))

    modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
    //println("Creating graphs")
    //createPNG("logs/flowsA1.dot", true)
    //createPNG("logs/flowsA2.dot", true)
    //createPNG("logs/flowsB.dot", true)
    println("Done")
end IncrementalRun

// Prints the maximal heap size.
object Memorycheck extends App:
    def formatSize(v: Long): String =
        if v < 1024 then return s"$v B"
        val z = (63 - java.lang.Long.numberOfLeadingZeros(v)) / 10
        s"${v.toDouble / (1L << (z * 10))} ${" KMGTPE".charAt(z)}B"

    println(formatSize(Runtime.getRuntime.nn.maxMemory()))

object IncrementalExtraction extends App:

    val text: String = "test/changes/scheme/generated/R5RS_icp_icp_1c_ontleed-4.scm"
    val version: Version = New

    val program = CSchemeParser.parseProgram(Reader.loadFile(text))
    println((if version == New then ProgramVersionExtracter.getUpdated(program) else ProgramVersionExtracter.getInitial(program)).prettyString())
