package maf.test.modular.scheme.incremental

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.test.*
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag
import maf.language.change.CodeVersion.*

import scala.concurrent.duration.*

/** Tests whether restarting an analysis after completion does not alter the result. */
trait IncrementalModXRestartTests extends IncrementalTestBase {

    /** Tests whether the analysis result does not change when the analysis is restarted after completion. */
    private def restartTest(program: SchemeExp): Unit =
        val a = analysis(program)
        a.configuration = ci_di_wi
        a.analyzeWithTimeout(analysisTimeout())
        assume(a.finished, "Initial analysis timed out.")

        // Initial analysis.
        var copy = a.deepCopy()
        copy.addToWorkList(copy.visited)
        copy.analyzeWithTimeout(analysisTimeout())
        assert(copy.finished, "Restart of initial analysis timed out unexpectedly.")
        checkEqState(a, copy, "Restarting initial analysis alters the analysis result.")

        // Incremental update.
        for c <- configurations do
            info(s"Checking $c.")
            copy = a.deepCopy()
            copy.configuration = c
            copy.updateAnalysis(analysisTimeout())
            if !copy.finished then alert(s"Updating timed out for $c.")
            else
                val updated = copy.deepCopy()
                copy.addToWorkList(copy.visited)
                copy.analyzeWithTimeout(analysisTimeout())
                assert(copy.finished, s"Restart of incremental update using $c timed out unexpectedly.")
                checkEqState(updated, copy, s"Restarting $c alters the analysis result.")
            end if
        end for //

        // Full reanalysis.
        val b = analysis(program)
        b.version = New
        b.configuration = noOptimisations
        b.analyzeWithTimeout(analysisTimeout())
        assume(b.finished, "Full reanalysis timed out.")
        copy = b.deepCopy()
        copy.addToWorkList(copy.visited)
        copy.analyzeWithTimeout(analysisTimeout())
        assert(copy.finished, "Restart of full reanalysis timed out unexpectedly.")
        checkEqState(b, copy, "Restarting full reanalysis alters the analysis result.")
    end restartTest

    override def onBenchmark(benchmark: Benchmark): Unit =
        property(s"Incremental analysis of $benchmark using ${name()} behaves sane.", testTags(benchmark): _*) {
            val content = Reader.loadFile(benchmark)
            val program = CSchemeParser.parseProgram(content)

            try
                // First check that the program analysis can be restarted without altering the result.
                restartTest(program)
            catch
                case e: VirtualMachineError =>
                    System.gc()
                    cancel(s"Analysis of $benchmark encountered an error: $e")
        }
}

class IncrementalModFTypeRestartTests extends IncrementalModXRestartTests with SequentialIncrementalBenchmarks:
    override def name() = "Incremental ModF Type"
    override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFAnalysisTypeLattice(b, ci_di_wi)

class IncrementalModFCPRestartTests extends IncrementalModXRestartTests with SequentialIncrementalBenchmarks:
    override def name() = "Incremental ModF CP"
    override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFAnalysisCPLattice(b, ci_di_wi)
