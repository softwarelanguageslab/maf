package maf.test.modular.scheme.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core.Identity
import org.scalatest.Tag
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.*
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.scheme.*
import maf.test.*
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timeout

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*

/**
 * Trait implementing soundness tests for incremental analyses.<br> Following properties are checked: <ul> <li>The soundness of the initial analysis
 * of the original program.</li> <li>The soundness of the incremental update of the program.</li> </ul> The properties are checked by comparing the
 * analysis results against the results of a concrete interpreter (run on both program versions). The comparisons are implemented in {@link
 * SchemeSoundnessTests}.
 *
 * @see
 *   SchemeSoundnessTests
 */
trait IncrementalModXSoundnessTests extends SchemeSoundnessTests:

    type IncrementalAnalysis = ModAnalysis[SchemeExp]
        with GlobalStore[SchemeExp]
        with ReturnValue[SchemeExp]
        with SchemeDomain
        with IncrementalModAnalysis[SchemeExp]
        with AnalysisResults[SchemeExp]

    def analysis(b: SchemeExp): IncrementalAnalysis

    override def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(3, MINUTES))

    val configurations: List[IncrementalConfiguration] = List(ci_di_wi) // The configurations to test.

    def runInterpreterWithVersion(
        i: SchemeInterpreter,
        p: SchemeExp,
        t: Timeout.T,
        version: Version
      ): ConcreteValues.Value = i.run(p, t, version)

    protected def evalConcreteWithVersion(
        program: SchemeExp,
        benchmark: Benchmark,
        version: Version
      ): Map[Identity, Set[Value]] =
        var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
        val timeout = concreteTimeout(benchmark)
        val times = concreteRuns(benchmark)
        try
            for _ <- 1 to times do
                val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)),
                                                        io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> ""))
                )
                runInterpreterWithVersion(interpreter, program, timeout, version)
        catch
            case _: TimeoutException =>
                alert(s"Concrete evaluation of $benchmark timed out.")
            case ProgramError(msg) =>
                alert(s"Concrete evaluation of $benchmark encountered a program error:\n$msg")
            case ChildThreadDiedException(_) =>
                alert(s"Concrete evaluation of $benchmark aborted due to a fatal crash in a child thread.")
            case e: VirtualMachineError =>
                System.gc()
                alert(s"Concrete evaluation of $benchmark failed with $e")
        idnResults

    protected def runAnalysisWithConfiguration(
        program: SchemeExp,
        benchmark: Benchmark,
        config: IncrementalConfiguration
      ): IncrementalAnalysis =
        try
            // analyze the program using a ModF analysis
            val anl = analysis(program)
            anl.configuration = config
            val timeout = analysisTimeout(benchmark)
            anl.analyzeWithTimeout(timeout)
            anl
        catch
            case e: VirtualMachineError =>
                System.gc()
                cancel(s"Analysis of $benchmark encountered an error: $e")
    // case InvalidConfigurationException(msg, config) =>
    //   info(s"Analysis of $benchmark cannot be run using $config: invalid configuration encountered.")

    // This is horrible code.
    override def onBenchmark(benchmark: Benchmark): Unit =
        property(s"Incremental analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {

            // load the benchmark program
            val content = Reader.loadFile(benchmark)
            val program = CSchemeParser.parseProgram(content)

            val cResultsOld = evalConcreteWithVersion(program, benchmark, Old)

            val anlOld = runAnalysisWithConfiguration(program, benchmark, allOptimisations)
            assume(anlOld.finished, "Initial analysis timed out.")

            // Check soundness on the original version of the program.
            info("Checking initial analysis results.")
            compareResults(anlOld, cResultsOld, "initial analysis")

            val cResultsNew = evalConcreteWithVersion(program, benchmark, New)

            for c <- configurations do
                //try {
                // Check soundness on the updated version of the program.
                info(s"Checking results of $c")
                val anlCopy = anlOld.deepCopy()
                anlCopy.configuration = c
                updateAnalysis(anlCopy, benchmark)
                compareResults(anlCopy, cResultsNew, c.toString)
            // } catch {
            //   case a: AssertionError      => throw new Exception(s"Assertion violation using ${c}.", a)
            //   case t: java.lang.Throwable => throw new Exception(s"Analysis error using ${c}.", t)
            // }
        }

    private def updateAnalysis(anl: IncrementalAnalysis, benchmark: Benchmark): Unit =
        try
            val timeout = analysisTimeout(benchmark)
            anl.updateAnalysis(timeout)
            assume(anl.finished, "Reanalysis timed out.")
        catch
            case e: VirtualMachineError =>
                System.gc()
                cancel(s"Reanalysis of $benchmark encountered an error: $e.")

    override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ IncrementalTest

// Note: this also includes ci_di_wi which is already handled by IncrementalSmallStepModConcType and IncrementalSmallStepModConcCP
trait RemainingConfigurationsWithWI extends IncrementalModXSoundnessTests:
    override val configurations: List[IncrementalConfiguration] = allConfigurations.toSet.filter(_.writeInvalidation).toList
    override def isSlow(b: Benchmark) = true

trait RemainingConfigurationsWithCIWithoutWI extends IncrementalModXSoundnessTests:
    override val configurations: List[IncrementalConfiguration] = allConfigurations.toSet.filterNot(_.writeInvalidation).filter(_.componentInvalidation).toList
    override def isSlow(b: Benchmark) = true

trait RemainingConfigurationsWithoutCIWI extends IncrementalModXSoundnessTests:
    override val configurations: List[IncrementalConfiguration] = allConfigurations.toSet.filterNot(_.writeInvalidation).filterNot(_.componentInvalidation).toList
    override def isSlow(b: Benchmark) = true

/** Implements soundness tests for an incremental ModConc analysis. */
class IncrementalSmallStepModConcType extends IncrementalModXSoundnessTests with ConcurrentIncrementalBenchmarks:
    override val configurations: List[IncrementalConfiguration] = List(ci_di_wi)
    def name = "Incremental ModConc Type"

    override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalModConcAnalysisTypeLattice(b, ci_di_wi) // allOptimisations)

    override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ SchemeModConcTest :+ SmallStepTest
    override def isSlow(b: Benchmark): Boolean =
        Set(
          "test/changes/cscheme/threads/actors.scm",
          "test/changes/cscheme/threads/crypt.scm",
          "test/changes/cscheme/threads/crypt2.scm",
          "test/changes/cscheme/threads/stm.scm"
        )(b)

/** Implements soundness tests for an incremental ModConc analysis. */
class IncrementalSmallStepModConcCP extends IncrementalSmallStepModConcType with IncrementalModXSoundnessTests:
    override def name = "Incremental ModConc CP"
    override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalModConcAnalysisCPLattice(b, ci_di_wi) // allOptimisations)
    override def isSlow(b: Benchmark): Boolean = true

class IncrementalSmallStepModConcTypeRemainingConfigsWithWI extends IncrementalSmallStepModConcType with RemainingConfigurationsWithWI
class IncrementalSmallStepModConcCPRemainingConfigsWithWI extends IncrementalSmallStepModConcCP with RemainingConfigurationsWithWI
class IncrementalSmallStepModConcTypeRemainingConfigsWithCIWithoutWI extends IncrementalSmallStepModConcType with RemainingConfigurationsWithCIWithoutWI
class IncrementalSmallStepModConcCPRemainingConfigsWithCIWithoutWI extends  IncrementalSmallStepModConcCP with RemainingConfigurationsWithCIWithoutWI
class IncrementalSmallStepModConcTypeRemainingConfigsWithoutWI extends IncrementalSmallStepModConcType with RemainingConfigurationsWithoutCIWI
class IncrementalSmallStepModConcCPRemainingConfigsWithoutWI extends IncrementalSmallStepModConcCP with RemainingConfigurationsWithoutCIWI

/** Implements soundness tests for an incremental ModF type analysis. */
class IncrementalModFType extends IncrementalModXSoundnessTests with SequentialIncrementalBenchmarks:
    def name = "Incremental ModF Type"

    override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFAnalysisTypeLattice(b, ci_di_wi)

    override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ SchemeModFTest :+ BigStepTest
    override def isSlow(b: Benchmark): Boolean =
        Set(
          "test/changes/scheme/multiple-dwelling (coarse).scm",
          "test/changes/scheme/multiple-dwelling (fine).scm",
          "test/changes/scheme/leval.scm",
          "test/changes/scheme/machine-simulator.scm",
          "test/changes/scheme/mceval-dynamic.scm",
          "test/changes/scheme/nboyer.scm",
          "test/changes/scheme/peval.scm"
        )(b) || IncrementalSchemeBenchmarkPrograms.sequentialGenerated(b)

/** Implements soundness tests for an incremental ModF CP analysis. */
class IncrementalModFCP extends IncrementalModFType:
    override def name = "Incremental ModF CP"
    override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFAnalysisCPLattice(b, ci_di_wi)
    override def isSlow(b: Benchmark): Boolean = true

class IncrementalModFTypeRemainingConfigsWithWI extends IncrementalModFType with RemainingConfigurationsWithWI
class IncrementalModFCPRemainingConfigsWithWI extends IncrementalModFCP with RemainingConfigurationsWithWI
class IncrementalModFTypeRemainingConfigsWithCIWithoutWI extends IncrementalModFType with RemainingConfigurationsWithCIWithoutWI
class IncrementalModFCPRemainingConfigsWithCIWithoutWI extends  IncrementalModFCP with RemainingConfigurationsWithCIWithoutWI
class IncrementalModFTypeRemainingConfigsWithoutCIWI extends IncrementalModFType with RemainingConfigurationsWithoutCIWI
class IncrementalModFCPRemainingConfigsWithoutCIWI extends IncrementalModFCP with RemainingConfigurationsWithoutCIWI
