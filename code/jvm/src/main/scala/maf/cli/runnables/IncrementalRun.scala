package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.incremental.*
import maf.core.Expression
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

    def storeDiff(a: A, b: A, aName: String = "a", bName: String = "b"): String =
        (a.store.keySet ++ b.store.keySet).foldLeft("") { case (str, addr) =>
            val valA = a.store.getOrElse(addr, a.lattice.bottom)
            val valB = b.store.getOrElse(addr, b.lattice.bottom)
            if valA != valB
            then
                val comp: String = (a.lattice.subsumes(valA, valB), a.lattice.subsumes(valB, valA)) match
                    case (true, true) => s"$aName == $bName"
                    case (false, false) => s"$aName ⋢ $bName ⋢ $aName"
                    case (true, false) => s"$bName ⊏ $aName"
                    case (false, true) => s"$aName ⊏ $bName"
                str ++ (addr.toString + s" ($comp)" + "\n" + a.lattice.compare(valA, valB, aName, bName) + "\n")
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

    def newTimeout(): Timeout.T = Timeout.start(Duration(3, MINUTES))

    // Check whether an analysis (in an incremental update) arrives in a loop with the same store).
    // Returns a tuple indicating steps in the analysis that are identical.
    // The check can happen for individual addresses or for all addresses in the store.
    def checkLoop(a: BaseModFAnalysisIncremental, adr: Set[String] = Set()): (Int, Int) = {
        var step: Int = 0
        a.analyzeWithTimeout(newTimeout())
        a.version = New
        if a.configuration.cyclicValueInvalidation then a.SCAs = a.computeSCAs(false)
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
    def analyse(text: SchemeExp, throwAssertionViolations: Boolean, logging: Boolean = true, images: Boolean = false, name: Option[String] = None): Boolean =
        try
            val init = if logging then new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations) else new IncrementalSchemeModFAnalysisTypeLatticeNoLogging(text, allOptimisations)
            val rean = if logging then new IncrementalSchemeModFAnalysisTypeLattice(text, allOptimisations) else new IncrementalSchemeModFAnalysisTypeLatticeNoLogging(text, allOptimisations)

            import SimpleTimer.*

            start()
            println(markStep("init"))
            init.analyzeWithTimeout(newTimeout())
            val incr = init.deepCopy() // Copy as making the image triggers SCC computations etc.
            if images then init.dataFlowToImage("flows-init.dot", name.map(_ + " (initial analysis)"))

            tick()
            println(markStep("rean"))
            rean.version = New
            rean.analyzeWithTimeout(newTimeout())
            if !rean.finished
            then
                println(markError("REAN timed out."))
                return false

            tick()
            println(markStep("upd"))
            incr.updateAnalysis(newTimeout())
            if !incr.finished
            then
                println(markError("INCR timed out."))
                return false

            stop()
            if images
            then
                incr.dataFlowToImage("flows-incr.dot", name.map(_ + " (incremental update)"))
                rean.dataFlowToImage("flows-rean.dot", name.map(_ + " (full reanalysis)"))
            println(markStep("Comparing analyses"))
            //a.logger.logU("store difference with full reanalysis:\n" ++ storeDiff(a, b)) // Log the difference in stores if any.
            val diff = storeDiff(incr, rean, "incr", "rean")
            println(diff)
            if throwAssertionViolations
            then
                println(markError(diff))
                assert(diff.isEmpty)
            //if unsound[SchemeExp](incr, rean) then throw new Exception(s"Unsound: $name")
            diff.isEmpty
            //!unsound[SchemeExp](incr, rean)
        catch
            case t: Throwable =>
                println(t.toString)
                throw t

    // Uses delta debugging to reduce a program to a minimal version that is still imprecise.
    def reduceImprecise(text: SchemeExp): SchemeExp = reduce(text, !analyse(_, false, false, false))

    def unsound[E <: Expression](inc: IncrementalModAnalysis[E] with IncrementalGlobalStoreCY[E],
                   rean: IncrementalModAnalysis[E] with IncrementalGlobalStoreCY[E]): Boolean =
        val allAddr = inc.store.keySet ++ rean.store.keySet
        allAddr.foreach({ a =>
            val incr = inc.store.getOrElse(a, inc.lattice.bottom)
            val rn = rean.store.getOrElse(a, rean.lattice.bottom)
            if incr != rn && !inc.lattice.subsumes(incr, rn.asInstanceOf[inc.Value])
            then return true
        })
        false

    List(
        //"test/DEBUG1.scm", // Gereduceerd van "test/changes/scheme/generated/R5RS_various_four-in-a-row-5.scm"
        //"test/DEBUG2.scm", // "test/changes/scheme/generated/R5RS_sigscheme_mem-1.scm"
        "test/DEBUG3.scm", // "test/changes/scheme/generated/R5RS_scp1_dedouble-2.scm"

      /*
        // Not precise yet.
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
        "test/changes/scheme/generated/R5RS_various_four-in-a-row-5.scm", */
    ).foreach { bench =>
        try {
            println(markTask(s"***** $bench *****"))
            val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
            val anly = true // Switch between analysing and delta debugging
            val logging = false
            val images = true

            // Update the flags above!
            if anly
            then
                println(text)
                val r = analyse(text, false, logging = logging && anly, images = images && anly, Some(bench))
                if r then println(greenText("PRECISE")) else println(redText("IMPRECISE"))
            else
                val reduced = reduceImprecise(text)
                println(reduced)
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
