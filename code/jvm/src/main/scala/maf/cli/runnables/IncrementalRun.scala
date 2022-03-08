package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.incremental.SplitPerformance
import maf.core.worklist.FIFOWorkList
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.{ProgramError, SchemeInterpreter}
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.{GlobalStore, ModAnalysis}
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.scheme.modf.*
import maf.modular.incremental.{IncrementalLogging, IncrementalModAnalysis, *}
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.ProgramVersionExtracter.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.{PrmAddr, SchemeTypeDomain}
import maf.modular.worklist.{FIFOWorklistAlgorithm, LIFOWorklistAlgorithm}
import maf.util.{Reader, Writer}
import maf.util.Writer.Writer
import maf.util.benchmarks.{Timeout, Timer}
import maf.util.graph.DotGraph
import maf.util.graph.DotGraph.*

import scala.concurrent.duration.*

object IncrementalRun extends App:

    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
      new IncrementalSchemeModFAnalysisTypeLattice(text, configuration)
      //with IncrementalLogging[SchemeExp]
      //with IncrementalDataFlowVisualisation[SchemeExp]
      {
        //override def focus(a: Addr): Boolean = false //a.toString.toLowerCase().nn.contains("ret")

        override def intraAnalysis(
            cmp: SchemeModFComponent
          ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
        //  with AssertionModFIntra
        //with IncrementalLoggingIntra
        //with IncrementalVisualIntra
      }

    // Performance benchmarks
    def perfAnalysis(e: SchemeExp, config: IncrementalConfiguration) = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
      with SplitPerformance[SchemeExp]
      with IncrementalLogging[SchemeExp] {
      mode = Mode.Summary
      var cnt = 0
      override def run(timeout: Timeout.T) =
          super.run(timeout)
          println(cnt)
      override def intraAnalysis(cmp: Component) =
        new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          with SplitPerformanceIntra
          with IncrementalLoggingIntra {
          override def analyzeWithTimeout(timeout: Timeout.T): Unit =
              cnt = cnt + 1
              super.analyzeWithTimeout(timeout)
        }
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

    def compareAnalyses(
        inc: IncrementalModAnalysis[SchemeExp] with GlobalStore[SchemeExp],
        rean: IncrementalModAnalysis[SchemeExp] with GlobalStore[SchemeExp],
        name: String
      ): Unit =
        val cName = inc.configuration.toString
        // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
        val iStore = inc.store.filterNot(kv => inc.lattice.isBottom(kv._2)).withDefaultValue(inc.lattice.bottom)
        val rStore = rean.store.filterNot(kv => rean.lattice.isBottom(kv._2)).withDefaultValue(rean.lattice.bottom)
        println(iStore.keySet.size)
        val allAddr = (iStore.keySet ++ rStore.keySet).filter(!_.isInstanceOf[PrmAddr])
        var e: Long = 0L
        var l: Long = 0L
        var m: Long = 0L
        allAddr.foreach({ a =>
            val incr = iStore(a)
            val rean = rStore(a)
            if incr == rean then e += 1 // Both results are the same => equally precise.
            else if inc.lattice.subsumes(incr, rean.asInstanceOf[inc.Value]) then l += 1 // The incremental value subsumes the value of the full reanalysis => less precise.
            else {
              //System.err.nn.println(s"$a: $incr < $rean") // Soundness error.
              //System.err.nn.flush()
              m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
            }
        })
        System.err.nn.println(s"$name: less precise: $l -- equal: $e -- more precise: $m")
    end compareAnalyses

    // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
    // Useful when reducing a program when debugging the analysis.
    def interpretProgram(file: String): Unit =
        val prog = CSchemeParser.parseProgram(Reader.loadFile(file))
        val i = new SchemeInterpreter((_, _) => (), stack = true)
        List(Old, New).foreach { version =>
          try
              print("*")
              i.run(prog, Timeout.start(Duration(3, MINUTES)), version)
          catch {
            case ProgramError(e) => System.err.nn.println(e)
          }
        }
        println("Done interpreting.")

    val modFbenchmarks: List[String] = List(
      "test/changes/scheme/slip-1-to-2.scm"
    )

    def newTimeout(): Timeout.T = Timeout.start(Duration(20, MINUTES))
    val configs = List(wi, ci_wi)

    modFbenchmarks.foreach { bench =>
      try {
        println(s"***** $bench *****")
        //interpretProgram(bench)
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))

        val a = newAnalysis(text, ci_di_wi)
        //a.logger.logU("***** INIT *****")
        a.analyzeWithTimeout(newTimeout())
        assert(a.finished)

        val noOpt = a.deepCopy()
        noOpt.configuration = noOptimisations
        //noOpt.logger.logU("***** NO OPT *****")
        noOpt.updateAnalysis(newTimeout())
        assert(noOpt.finished)

        val full = newAnalysis(text, noOptimisations)
        full.version = New
        //full.logger.logU("***** FULL *****")
        full.analyzeWithTimeout(newTimeout())
        assert(full.finished)

        configs.foreach { config =>
            val opt = a.deepCopy()
            opt.configuration = config
            //opt.logger.logU(s"***** ${config.toString} *****")
            opt.updateAnalysis(newTimeout())
            assert(opt.finished)

        compareAnalyses(opt, full, s"${opt.configuration.toString} vs. Full")
        //compareAnalyses(opt, noOpt, s"${opt.configuration.toString} vs. No Opt")
        }

      } catch {
        case e: Exception =>
          e.printStackTrace(System.out)
      }
    }

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

    val text: String = "test/changes/scheme/slip-1-to-2.scm"
    val version: Version = New

    val program = CSchemeParser.parseProgram(Reader.loadFile(text))
    println((if version == New then ProgramVersionExtracter.getUpdated(program) else ProgramVersionExtracter.getInitial(program)).prettyString())
