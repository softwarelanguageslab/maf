package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.incremental.*
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
import maf.modular.worklist.{LIFOWorklistAlgorithm, *}
import maf.util.*
import maf.util.ColouredFormatting.*
import maf.util.Writer.Writer
import maf.util.benchmarks.*
import maf.util.graph.{DotGraph, Graph}

import scala.concurrent.duration.*

object IncrementalRun extends App:

    type A = ModAnalysis[SchemeExp] with IncrementalGlobalStoreCY[SchemeExp] with IncrementalSchemeTypeDomain

    class Analysis(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends ModAnalysis[SchemeExp](prg)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsM
        with IncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeTypeDomain
        with IncrementalGlobalStoreCY[SchemeExp]
        with IncrementalLogging[SchemeExp]
        with LIFOWorklistAlgorithm[SchemeExp]
        with IncrementalDataFlowVisualisation[SchemeExp] {
        override def focus(a: Addr): Boolean = a.toString.contains("x@54:20")
        mode = Mode.Coarse // Mode.Step // Mode.Select
        stepFocus = 1 //13//10//23//119
        override def warn(msg: String): Unit = ()
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp)
                with IncrementalSchemeModFBigStepIntra
                with IncrementalGlobalStoreCYIntraAnalysis
                with IncrementalLoggingIntra
                with IncrementalVisualIntra
    }

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

    def checkEqState(a: A, b: A, message: String = ""): Unit =
        (a.store.keySet ++ b.store.keySet).foreach { addr =>
            val valA = a.store.getOrElse(addr, a.lattice.bottom)
            val valB = b.store.getOrElse(addr, b.lattice.bottom)
            if valA != valB then System.err.nn.println(addr.toString + "\n" + a.lattice.compare(valA, valB))
        }
        assert(a.store.filterNot(_._2 == a.lattice.bottom) == b.store.filterNot(_._2 == b.lattice.bottom), message + " (store mismatch)")
        assert(a.visited == b.visited, message + " (visited set mismatch)")
        (a.deps.keySet ++ b.deps.keySet).foreach { dep =>
            val dA = a.deps.getOrElse(dep, Set())
            val dB = b.deps.getOrElse(dep, Set())
            if dA != dB then System.err.nn.println(dep.toString + "\n" + dA.mkString(" ") + "\n" + dB.mkString(" "))
        }
    // println(a.deps.toList.map(_.toString).sorted)
    // println(b.deps.toList.map(_.toString).sorted)
    /* assert(a.deps == b.deps, message + " (dependency mismatch)")
        assert(a.mapping == b.mapping, message + " (mapping mismatch)")
        assert(a.cachedReadDeps == b.cachedReadDeps, message + " (read deps mismatch)")
        assert(a.cachedSpawns == b.cachedSpawns, message + " (spawns mismatch)")
        assert(a.provenance == b.provenance, message + " (provenance mismatch)")
        assert(a.cachedWrites == b.cachedWrites, message + " (write cache mismatch)")
        //assert(a.implicitFlows == b.implicitFlows, message + " (flow mismatch)") // TODO Readd?
        assert(a.dataFlowR == b.dataFlowR, message + " (reverse flow mismatch)") */

    class IncrementalSchemeModFAnalysisTypeLattice(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends BaseModFAnalysisIncremental(prg)
            with IncrementalSchemeTypeDomain
            with IncrementalLogging[SchemeExp]
            with IncrementalDataFlowVisualisation[SchemeExp]
            with IncrementalGlobalStoreCY[SchemeExp]:
        override def focus(a: Addr): Boolean = false //!a.toString.contains("PrmAddr")
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis with IncrementalLoggingIntra with IncrementalVisualIntra

    val modFbenchmarks: List[String] = List(
      //  "test/changes/scheme/leval.scm", // Resulteert in errors (andere bench ook). => heapSpace error
      "test/changes/scheme/freeze.scm" // Nog niet precies.
    )

    def newTimeout(): Timeout.T = Timeout.start(Duration(20, MINUTES))

    modFbenchmarks.foreach { bench =>
        try {
            println(markTask(s"***** $bench *****"))
            val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
            val a = new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations)
            val b = new IncrementalSchemeModFAnalysisTypeLattice(text, noOptimisations)

            println(text.prettyString())

            println("init")
            a.analyzeWithTimeout(newTimeout())
            b.version = New
            b.analyzeWithTimeout(newTimeout())

            println("upd")
            a.computeSCAs().foreach(s => println(s.mkString(" ")))
            a.updateAnalysis(newTimeout())

            a.dataFlowToImage("flows.dot")
            checkEqState(a, b)
        } catch {
            case e: Exception =>
                e.printStackTrace(System.out)
        }
    }
    println(markOK("\n\n**Done**\n\n"))
end IncrementalRun

// Prints the maximal heap size.
object JVMMemorySize extends App:
    def formatSize(v: Long): String =
        if v < 1024 then return s"$v B"
        val z = (63 - java.lang.Long.numberOfLeadingZeros(v)) / 10
        s"${v.toDouble / (1L << (z * 10))} ${" KMGTPE".charAt(z)}B"

    println(formatSize(Runtime.getRuntime.nn.maxMemory()))

object IncrementalExtraction extends App:

    val text: String = "test/changes/scheme/satCoarse.scm"
    val version: Version = New

    val program = CSchemeParser.parseProgram(Reader.loadFile(text))
    println((if version == New then ProgramVersionExtracter.getUpdated(program) else ProgramVersionExtracter.getInitial(program)).prettyString())
