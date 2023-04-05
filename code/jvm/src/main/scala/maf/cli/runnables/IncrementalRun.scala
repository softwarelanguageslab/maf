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

import scala.concurrent.duration.*

object IncrementalRun extends App:

    type A = ModAnalysis[SchemeExp] with IncrementalGlobalStoreCY[SchemeExp] with IncrementalSchemeTypeDomain

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
            if valA != valB
            then str ++ (addr.toString + "\n" + a.lattice.compare(valA, valB) + "\n") else str
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

    class IncrementalSchemeModFAnalysisTypeLattice(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends BaseModFAnalysisIncremental(prg)
            with IncrementalSchemeTypeDomain
            with IncrementalLogging[SchemeExp]
            with IncrementalDataFlowVisualisation[SchemeExp]
            with IncrementalGlobalStoreCY[SchemeExp]:
        stepFocus = Set(25,26,27)
        override def focus(a: Addr): Boolean = !a.toString.contains("PrmAddr") /* List("env@35:33[Some(ε)]",
            "env@39:40[Some(ε)]",
            "exp@11:29[Some(ε)]",
            "exp@15:26[Some(ε)]",
            "exp@35:29[Some(ε)]",
            "exp@41:34[Some(ε)]",
            "exp@43:29[Some(ε)]",
            "exp@45:31[Some(ε)]",
            "exp@47:25[Some(ε)]",
            "exp@49:29[Some(ε)]",
            "exp@51:28[Some(ε)]",
            "exps@39:35[Some(ε)]",
            "output@58:26[Some(ε)]",
            "ret (λ@11:21 [ε])",
            "ret (λ@15:18 [ε])",
            "ret (λ@35:21 [ε])",
            "ret (λ@39:27 [ε])",
            "ret (λ@41:26 [ε])",
            "ret (λ@43:21 [ε])",
            "ret (λ@45:23 [ε])",
            "ret (λ@47:17 [ε])",
            "ret (λ@49:21 [ε])",
            "ret (λ@51:20 [ε])",
            "tag@41:38[Some(ε)]").contains(a.toString) */

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

    def newTimeout(): Timeout.T = Timeout.start(Duration(10, SECONDS))

    // Check whether an analysis (in an incremental update) arrives in a loop with the same store).
    // Returns a tuple indicating steps in the analysis that are identical.
    // The check can happen for individual addresses or for all addresses in the store.
    def checkLoop(a: BaseModFAnalysisIncremental, adr: Set[String] = Set()): (Int, Int) = {
        var step: Int = 0
        a.analyzeWithTimeout(newTimeout())
        a.version = New
        if a.configuration.cyclicValueInvalidation then a.SCAs = a.computeSCAs()
        val affected = a.findUpdatedExpressions(a.program).flatMap(a.mapping)
        affected.foreach(a.addToWorkList)
        var anly: Map[Int, BaseModFAnalysisIncremental] = Map() + (0 -> a)
        val t = Timeout.start(Duration(1000, SECONDS))
        def identical(an1: BaseModFAnalysisIncremental, an2: BaseModFAnalysisIncremental): Boolean =
            if adr.isEmpty
            then an1.store == an2.store
            else adr.forall { ad =>
                an1.store.find(a => ad == a.toString).getOrElse(an1.lattice.bottom) ==  an2.store.find(a => ad == a.toString).getOrElse(an2.lattice.bottom)
            }
        while !a.finished && !t.reached do
            a.step(t)
            anly.find(t => identical(t._2, a)) match {
                case Some((n, _)) if anly.toList.drop(n+1).exists(t => !identical(t._2, a)) => return (n, step) // Store should have changed in the meantime.
                case _ =>
                    anly = anly + (step -> a)
                    step = step + 1
            }
        println(s"Checked $step steps.")
        (-1, -1)
    }

    def reduce(text: SchemeExp, oracle: SchemeExp => Boolean): SchemeExp =
        val log = Logger.raw("reduced-program")
        import SimpleTimer.*

        val exp = GTR.reduce(text, oracle, identity, TransformationManager.allTransformations)
        log.log(exp.prettyString())
        exp

    def analyse(text: SchemeExp, throwAssertionViolations: Boolean, logging: Boolean = true): Boolean = // Returns whether the analysis is fully precise.
        try
            val a = if logging then new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations) else new IncrementalSchemeModFAnalysisTypeLatticeNoLogging(text, allOptimisations)
            val b = if logging then new IncrementalSchemeModFAnalysisTypeLattice(text, noOptimisations) else new IncrementalSchemeModFAnalysisTypeLatticeNoLogging(text, noOptimisations)

            // println(text.prettyString())
            import SimpleTimer.*

            // println(text.prettyString())

            start()
            println(markStep("init"))
            a.analyzeWithTimeout(newTimeout())

            //tick()
            //println(markStep("Generating svg file."))
            //a.dataFlowToImage("flows-init.dot")

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

            stop()
            println(markStep("Comparing analyses"))
            //a.logger.logU("store difference with full reanalysis:\n" ++ storeDiff(a, b)) // Log the difference in stores if any.
            if throwAssertionViolations
            then
                checkEqState(a, b)
                true // Throw exceptions when things don't match.
            else !storeDiff(a, b).contains("+-")
        catch
            case t: Throwable =>
                println(t.toString)
                throw t

    List(
        //  "test/changes/scheme/leval.scm", // Resulteert in errors (andere bench ook). => heapSpace error
        //"test/changes/scheme/freeze.scm" // Nog niet precies.
        "test/DEBUG2.scm",
        //"logs/reduced-program.txt"
       // "test/changes/scheme/generated/R5RS_WeiChenRompf2019_the-little-schemer_ch3-5.scm"
    ).foreach { bench =>
        try {
            println(markTask(s"***** $bench *****"))
            val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
            analyse(text, true, true)
          //  reduce(text, { (text: SchemeExp) =>
          //    !analyse(text, false, false)
          //  } )
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
