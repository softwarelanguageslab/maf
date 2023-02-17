package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.incremental.*
import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.GTR.*
import maf.deltaDebugging.gtr.transformations.TransformationManager
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
        override def focus(a: Addr): Boolean =
            List(
                "exp@164:22[Some(ε)]",
                "PtrAddr((__toplevel_cons 'c ()))[None]",
                "ops@278:24[Some(ε)]",
                "PtrAddr((car args))[Some(ε)]",
                "exp@34:13[Some(ε)]"
            ).exists(s => a.toString.contains(s))
        mode = Mode.Summary // Mode.Step // Mode.Select
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

    def storeDiff(a: A, b: A): String =
        (a.store.keySet ++ b.store.keySet).foldLeft("") { case (str, addr) =>
            val valA = a.store.getOrElse(addr, a.lattice.bottom)
            val valB = b.store.getOrElse(addr, b.lattice.bottom)
            if valA != valB then str ++ (addr.toString + "\n" + a.lattice.compare(valA, valB) + "\n") else str
        }

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
        override def focus(a: Addr): Boolean = true
        mode = Mode.Fine
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis with IncrementalLoggingIntra with IncrementalVisualIntra

    class IncrementalSchemeModFAnalysisTypeLatticeNoLogging(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends BaseModFAnalysisIncremental(prg)
            with IncrementalSchemeTypeDomain
            with IncrementalDataFlowVisualisation[SchemeExp]
            with IncrementalGlobalStoreCY[SchemeExp]:

        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis with IncrementalVisualIntra


    val modFbenchmarks: List[String] = List(
      //  "test/changes/scheme/leval.scm", // Resulteert in errors (andere bench ook). => heapSpace error
       //"test/changes/scheme/freeze.scm" // Nog niet precies.
       "test/DEBUG3.scm"
    )

    def newTimeout(): Timeout.T = Timeout.start(Duration(20, MINUTES))

    def oracle(e: SchemeExp): Boolean =
        abstract class BaseModFAnalysisIncremental(prg: SchemeExp, var configuration: IncrementalConfiguration)
            extends ModAnalysis[SchemeExp](prg)
                with StandardSchemeModFComponents
                with SchemeModFNoSensitivity
                with SchemeModFSemanticsM
                with IncrementalSchemeModFBigStepSemantics
                with IncrementalSchemeTypeDomain
                with IncrementalGlobalStoreCY[SchemeExp] {
            override def warn(msg: String): Unit = ()

            override def intraAnalysis(cmp: Component) =
                new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis
        }

        val lifo = new BaseModFAnalysisIncremental(e, ci) with LIFOWorklistAlgorithm[SchemeExp]
        val fifo = new BaseModFAnalysisIncremental(e, ci) with FIFOWorklistAlgorithm[SchemeExp]

        // Initial analysis.
        lifo.analyzeWithTimeout(newTimeout())
        assume(lifo.finished, "Initial LIFO analysis timed out.")
        fifo.analyzeWithTimeout(newTimeout())
        assume(fifo.finished, "Initial FIFO analysis timed out.")

        checkEqState(lifo, fifo, "Initial analysis is not equal when using LIFO and FIFO.")

        lifo.updateAnalysis(newTimeout())
        fifo.updateAnalysis(newTimeout())
        if !lifo.finished then println("LIFO did not finish incremental update.")
        if !fifo.finished then println("FIFO did not finish incremental update.")
        if lifo.finished && fifo.finished
        then
            storeDiff(lifo, fifo).contains("+-")
        else
            println(markWarning("No comparison."))
            false
    end oracle

    def reduce(bench: String): SchemeExp =
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        val log = Logger.numbered()
        import SimpleTimer.*

        val exp = GTR.reduce(text, oracle, identity, TransformationManager.allTransformations)
        log.logU(exp.prettyString())
        exp

    def analyse(bench: String): Unit =
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        val a = new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations)
        val b = new IncrementalSchemeModFAnalysisTypeLattice(text, noOptimisations)

        // println(text.prettyString())
        import SimpleTimer.*

        start()
        println(markStep("init"))
        a.analyzeWithTimeout(newTimeout())

        tick()
        println(markStep("rean"))
        b.version = New
        b.analyzeWithTimeout(newTimeout())

        tick()
        println(markStep("upd"))
        a.updateAnalysis(newTimeout())

        //tick()
        //println(markStep("Generating svg file."))
        //a.dataFlowToImage("flows.dot")

        tick()
        println(markStep("Comparing analyses"))
        a.logger.logU("store difference with full reanalysis:\n" ++ storeDiff(a, b)) // Log the difference in stores if any.
        checkEqState(a, b) // Throw exceptions when things don't match.
        stop()

    modFbenchmarks.foreach { bench =>
        try {
            println(markTask(s"***** $bench *****"))
            val text = CSchemeParser.parseProgram(Reader.loadFile(bench))

            abstract class BaseModFAnalysisIncremental(prg: SchemeExp, var configuration: IncrementalConfiguration)
                extends ModAnalysis[SchemeExp](prg)
                    with StandardSchemeModFComponents
                    with SchemeModFNoSensitivity
                    with SchemeModFSemanticsM
                    with IncrementalSchemeModFBigStepSemantics
                    with IncrementalSchemeTypeDomain
                    with IncrementalLogging[SchemeExp]
                    with IncrementalGlobalStoreCY[SchemeExp] {
                override def warn(msg: String): Unit = ()

                override def focus(a: Addr): Boolean = a.toString == "ret (λ@8:36 [ε])"

                override def intraAnalysis(cmp: Component) =
                    new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis with IncrementalLoggingIntra
            }

            val lifo = new BaseModFAnalysisIncremental(text, ci) with LIFOWorklistAlgorithm[SchemeExp]
            val fifo = new BaseModFAnalysisIncremental(text, ci) with FIFOWorklistAlgorithm[SchemeExp]

            // Initial analysis.
            lifo.analyzeWithTimeout(newTimeout())
            assume(lifo.finished, "Initial LIFO analysis timed out.")
            fifo.analyzeWithTimeout(newTimeout())
            assume(fifo.finished, "Initial FIFO analysis timed out.")

            lifo.updateAnalysis(newTimeout())
            fifo.updateAnalysis(newTimeout())
            if !lifo.finished then println("LIFO did not finish incremental update.")
            if !fifo.finished then println("FIFO did not finish incremental update.")
            println(storeDiff(lifo, fifo))
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
