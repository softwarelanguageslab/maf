package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.incremental.SplitPerformance
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.*
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.scheme.modf.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.*
import maf.modular.worklist.*
import maf.util.*
import maf.util.Writer.Writer
import maf.util.benchmarks.*

import scala.concurrent.duration.*

object IncrementalRun extends App:

    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
        new IncrementalSchemeModFAnalysisCPLattice(text, configuration)
            with IncrementalLogging[SchemeExp]
            //with IncrementalDataFlowVisualisation[SchemeExp]
            {
            override def focus(a: Addr): Boolean = a.toString == "VarAddr(x@<=:1:13)[Some(ε)]"
            mode = Mode.Coarse
            override def intraAnalysis(
                cmp: SchemeModFComponent
              ) = new IntraAnalysis(cmp)
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

    abstract class BaseModFAnalysisIncremental(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends ModAnalysis[SchemeExp](prg)
            with StandardSchemeModFComponents
            with SchemeModFNoSensitivity
            with SchemeModFSemanticsM
            with IncrementalSchemeModFBigStepSemantics
            with IncrementalSchemeConstantPropagationDomain
            with IncrementalGlobalStore[SchemeExp]
            with IncrementalLogging[SchemeExp]
            {
         override def focus(a: Addr): Boolean = a.toString.contains("ret (zero? [ε])")
        override def warn(msg: String): Unit = ()
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with IncrementalLoggingIntra
    }

    def lifoAnalysis(b: SchemeExp) = new BaseModFAnalysisIncremental(b, ci_di_wi) with LIFOWorklistAlgorithm[SchemeExp]
    def fifoAnalysis(b: SchemeExp) = new BaseModFAnalysisIncremental(b, ci_di_wi) with FIFOWorklistAlgorithm[SchemeExp]

    // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
    // Useful when reducing a program when debugging the analysis.
    def interpretProgram(file: String): Unit =
        val prog = CSchemeParser.parseProgram(Reader.loadFile(file))
        val i = new SchemeInterpreter((_, _) => ())
        List(Old, New).foreach { version =>
            try
                print("*")
                i.run(prog, Timeout.start(Duration(3, MINUTES)), version)
            catch {
                case ProgramError(e) => System.err.nn.println(e)
            }
        }
        println("Done interpreting.")

    def checkEqState(a: BaseModFAnalysisIncremental, b: BaseModFAnalysisIncremental, message: String): Unit =
        assert(a.store == b.store, message + " (store mismatch)")
        assert(a.visited == b.visited, message + " (visited set mismatch)")
        assert(a.deps == b.deps, message + " (dependency mismatch)")
        assert(a.mapping == b.mapping, message + " (mapping mismatch)")
        assert(a.cachedReadDeps == b.cachedReadDeps, message + " (read deps mismatch)")
        assert(a.cachedSpawns == b.cachedSpawns, message + " (spawns mismatch)")
        assert(a.provenance == b.provenance, message + " (provenance mismatch)")
        assert(a.cachedWrites == b.cachedWrites, message + " (write cache mismatch)")
        //assert(a.implicitFlows == b.implicitFlows, message + " (flow mismatch)") // TODO Readd?
        assert(a.dataFlowR == b.dataFlowR, message + " (reverse flow mismatch)")

    val modFbenchmarks: List[String] = List(
      "test/DEBUG1.scm"
    )

    def newTimeout(): Timeout.T = Timeout.start(Duration(20, MINUTES))
    val configs = List(noOptimisations)

    modFbenchmarks.foreach { bench =>
        try {
            println(s"***** $bench *****")
            //interpretProgram(bench)
            val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
            println(text.prettyString())

            val l = lifoAnalysis(text)
            l.configuration = ci_di_wi
            val f = fifoAnalysis(text)
            f.configuration = ci_di_wi
            //a.logger.logU("***** INIT *****")
            l.analyzeWithTimeout(newTimeout())
            f.analyzeWithTimeout(newTimeout())
            assert(l.finished)
            assert(f.finished)

            l.configuration = wi
            f.configuration = wi

            l.updateAnalysis(newTimeout())
            f.updateAnalysis(newTimeout())

            checkEqState(f, l,"")

            //val noOpt = a.deepCopy()
            //noOpt.configuration = noOptimisations
            //noOpt.logger.logU("***** NO OPT *****")
            //noOpt.updateAnalysis(newTimeout())
            //assert(noOpt.finished)

            //val full = newAnalysis(text, noOptimisations)
            //full.version = New
            //full.logger.logU("***** FULL *****")
            //full.analyzeWithTimeout(newTimeout())
            //assert(full.finished)
            /*
            configs.foreach { config =>
                println(config)
                val opt = l.deepCopy()
                opt.configuration = config
                //opt.logger.logU(s"***** ${config.toString} *****")
                opt.updateAnalysis(newTimeout())
                assert(opt.finished)

            // compareAnalyses(opt, full, s"${opt.configuration.toString} vs. Full")
            //compareAnalyses(opt, noOpt, s"${opt.configuration.toString} vs. No Opt")
            }
            */

        } catch {
            case e: Exception =>
                e.printStackTrace(System.out)
        }
    }

    println("Done")
end IncrementalRun

// Prints the maximal heap size.
object JVMMemorySize extends App:
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
