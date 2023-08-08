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

    def storeDiff(a: A, b: A): String =
        (a.store.keySet ++ b.store.keySet).foldLeft("") { case (str, addr) =>
            val valA = a.store.getOrElse(addr, a.lattice.bottom)
            val valB = b.store.getOrElse(addr, b.lattice.bottom)
            if valA != valB
            then str ++ (addr.toString + "\n" + a.lattice.compare(valA, valB) + "\n")
            else str
        }

    class IncrementalSchemeModFAnalysisTypeLatticeNoLogging(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends BaseModFAnalysisIncremental(prg)
            with IncrementalSchemeTypeDomain
            with IncrementalDataFlowVisualisation[SchemeExp]
            with IncrementalGlobalStoreCY[SchemeExp]:

        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis

    class IncrementalSchemeModFAnalysisTypeLattice(prg: SchemeExp, configuration: IncrementalConfiguration)
        extends IncrementalSchemeModFAnalysisTypeLatticeNoLogging(prg, configuration) with IncrementalLogging[SchemeExp]:

        mode = Mode.Fine
        override def focus(a: Addr): Boolean = !a.toString.contains("Prm")
        stepFocus = Set(25, 26, 27)

        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis with IncrementalLoggingIntra

    def newTimeout(): Timeout.T = Timeout.start(Duration(1, MINUTES))

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
        println(exp.prettyString())
        println(oracle(exp).toString)
        exp

    // Returns a boolean indicating whether the analysis is fully precise.
    def analyse(text: SchemeExp, throwAssertionViolations: Boolean, logging: Boolean = true): Boolean =
        try
            val a = if logging then new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations) else new IncrementalSchemeModFAnalysisTypeLatticeNoLogging(text, allOptimisations)
            val b = if logging then new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations) else new IncrementalSchemeModFAnalysisTypeLatticeNoLogging(text, allOptimisations)

            import SimpleTimer.*

            start()
            println(markStep("init"))
            a.analyzeWithTimeout(newTimeout())
            a.dataFlowToImage("flows-init.dot")

            tick()
            println(markStep("rean"))
            b.version = New
            b.analyzeWithTimeout(newTimeout())

            tick()
            println(markStep("upd"))
            a.updateAnalysis(newTimeout())

            stop()
            a.dataFlowToImage("flows-incr.dot")
            b.dataFlowToImage("flows-rean.dot")
            println(markStep("Comparing analyses"))
            //a.logger.logU("store difference with full reanalysis:\n" ++ storeDiff(a, b)) // Log the difference in stores if any.
            val diff = storeDiff(a, b)
            println(diff)
            if throwAssertionViolations
            then
                println(markError(diff))
                assert(diff.isEmpty)
            diff.isEmpty
        catch
            case t: Throwable =>
                println(t.toString)
                throw t

    // Uses delta debugging to reduce a program to a minimal version that is still imprecise.
    def reduceImprecise(text: SchemeExp): SchemeExp = reduce(text, !analyse(_, false, false))

    List(
        // Different results with and without LitAddr.
        "test/changes/scheme/generated/R5RS_gambit_matrix-1.scm",
        "test/changes/scheme/generated/R5RS_scp1_draw-umbrella-4.scm",
        "test/changes/scheme/generated/R5RS_scp1_draw-umbrella-5.scm",
        "test/changes/scheme/generated/R5RS_scp1_insert-2.scm",
        "test/changes/scheme/generated/R5RS_scp1_list-compare-n-1.scm",
        "test/changes/scheme/generated/R5RS_scp1_list-compare-n-3.scm",
        "test/changes/scheme/generated/R5RS_various_work-1.scm",
        "test/changes/scheme/generated/R5RS_various_work-3.scm",

        // Not precise yet.
        "test/DEBUG2.scm",
        "test/changes/scheme/generated/R5RS_WeiChenRompf2019_the-little-schemer_ch3-5.scm",
        "test/changes/scheme/generated/R5RS_gabriel_puzzle-4.scm",
        "test/changes/scheme/generated/R5RS_scp1_all-but-interval-5.scm",
        "test/changes/scheme/generated/R5RS_scp1_count-pairs2-1.scm",
        "test/changes/scheme/generated/R5RS_scp1_dedouble-2.scm",
        "test/changes/scheme/generated/R5RS_scp1_deep-map-combine-4.scm",
        "test/changes/scheme/generated/R5RS_scp1_merge-1.scm",
        "test/changes/scheme/generated/R5RS_scp1_merge-3.scm",
        "test/changes/scheme/generated/R5RS_scp1_merge-5.scm",
        "test/changes/scheme/generated/R5RS_sigscheme_mem-1.scm",
        "test/changes/scheme/generated/R5RS_various_church-4.scm",
        "test/changes/scheme/generated/R5RS_various_four-in-a-row-5.scm",
    ).slice(6,7).foreach { bench =>
        try {
            println(markTask(s"***** $bench *****"))
            val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
            println(text)
            println(!analyse(text, false, true))
            //val reduced = reduceImprecise(text)
            //println(reduced)
            //println(!analyse(reduced, true, true))
            //println(!analyse(CSchemeParser.parseProgram(Reader.loadFile("logs/reduced-program.txt")), false, true))
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
