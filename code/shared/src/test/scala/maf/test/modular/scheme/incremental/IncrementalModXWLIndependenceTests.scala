package maf.test.modular.scheme.incremental
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf.*
import maf.modular.worklist.*
import maf.test.SequentialIncrementalBenchmarks
import maf.util.Reader

/** Tests whether the fixed-point obtained by the analysis is independent of the work list order. */
trait IncrementalModXWLIndependenceTests extends IncrementalTestBase {

    override def analysis(b: SchemeExp): IncrementalAnalysis = throw new Exception("Use specialised methods.")
    def lifoAnalysis(b: SchemeExp): IncrementalAnalysis
    def fifoAnalysis(b: SchemeExp): IncrementalAnalysis
    def randAnalysis(b: SchemeExp): IncrementalAnalysis

    private def workListTest(program: SchemeExp): Unit =
        val lifo = lifoAnalysis(program)
        val fifo = fifoAnalysis(program)
        val rand = randAnalysis(program)
        lifo.configuration = ci_di_wi
        fifo.configuration = ci_di_wi
        rand.configuration = ci_di_wi

        // Initial analysis.
        info("Checking initial analysis.")
        lifo.analyzeWithTimeout(analysisTimeout())
        assume(lifo.finished, "Initial LIFO analysis timed out.")
        fifo.analyzeWithTimeout(analysisTimeout())
        assume(fifo.finished, "Initial FIFO analysis timed out.")
        rand.analyzeWithTimeout(analysisTimeout())
        assume(rand.finished, "Initial RAND analysis timed out.")

        checkEqState(lifo, fifo, "Initial analysis is not equal when using LIFO and FIFO.")
        checkEqState(lifo, rand, "Initial analysis is not equal when using LIFO and RAND.")

        // Incremental update.
        for c <- configurations do
            info(s"Checking $c.")
            val lifoCopy = lifo.deepCopy()
            val fifoCopy = fifo.deepCopy()
            val randCopy = rand.deepCopy()
            lifoCopy.configuration = c
            fifoCopy.configuration = c
            randCopy.configuration = c
            lifoCopy.updateAnalysis(analysisTimeout())
            fifoCopy.updateAnalysis(analysisTimeout())
            randCopy.updateAnalysis(analysisTimeout())
            if !lifoCopy.finished then alert("LIFO did not finish incremental update.")
            if !fifoCopy.finished then alert("FIFO did not finish incremental update.")
            if !randCopy.finished then alert("RAND did not finish incremental update.")
            (lifoCopy.finished, fifoCopy.finished, randCopy.finished) match {
                case (true, true, true) =>
                    checkEqState(lifoCopy, fifoCopy, s"Incremental update using $c is not equal when using LIFO and FIFO.")
                    checkEqState(lifoCopy, randCopy, s"Incremental update using $c is not equal when using LIFO and RAND.")
                case (true, true, false) =>
                    checkEqState(lifoCopy, fifoCopy, s"Incremental update using $c is not equal when using LIFO and FIFO.")
                case (true, false, true) =>
                    checkEqState(lifoCopy, randCopy, s"Incremental update using $c is not equal when using LIFO and RAND.")
                case (false, true, true) =>
                    checkEqState(fifoCopy, randCopy, s"Incremental update using $c is not equal when using FIFO and RAND.")
                case _ => alert("Not enough incremental updates succeeded to compare.")
            }
        end for //

        // Full reanalysis.
        info("Checking full reanalysis.")
        val lifo2 = lifoAnalysis(program)
        val fifo2 = fifoAnalysis(program)
        val rand2 = randAnalysis(program)
        lifo2.version = New
        fifo2.version = New
        rand2.version = New
        lifo2.configuration = noOptimisations
        fifo2.configuration = noOptimisations
        rand2.configuration = noOptimisations

        lifo2.analyzeWithTimeout(analysisTimeout())
        assume(lifo2.finished, "Full LIFO reanalysis timed out.")
        fifo2.analyzeWithTimeout(analysisTimeout())
        assume(fifo2.finished, "Full FIFO reanalysis timed out.")
        rand2.analyzeWithTimeout(analysisTimeout())
        assume(rand2.finished, "Full RAND reanalysis timed out.")

        checkEqState(lifo2, fifo2, "Full reanalysis is not equal when using LIFO and FIFO.")
        checkEqState(lifo2, rand2, "Full reanalysis is not equal when using LIFO and RAND.")

    end workListTest

    override def onBenchmark(benchmark: Benchmark): Unit =
        property(s"The result of the incremental analysis of $benchmark using ${name()} is independent of the work list order.",
                 testTags(benchmark): _*
        ) {
            val content = Reader.loadFile(benchmark)
            val program = CSchemeParser.parseProgram(content)

            try
                // First check that the program analysis can be restarted without altering the result.
                workListTest(program)
            catch
                case e: VirtualMachineError =>
                    System.gc()
                    cancel(s"Analysis of $benchmark encountered an error: $e")
        }
}

trait IncrementalModFCPWLIndependenceTests extends IncrementalModXWLIndependenceTests with SequentialIncrementalBenchmarks:
    override def name(): String = "Incremental ModF CP"
    abstract class BaseModFAnalysisIncremental(prg: SchemeExp, var configuration: IncrementalConfiguration)
        extends ModAnalysis[SchemeExp](prg)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsM
        with IncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeConstantPropagationDomain
        with IncrementalGlobalStore[SchemeExp] {
        override def warn(msg: String): Unit = ()
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
    }

    def lifoAnalysis(b: SchemeExp): IncrementalAnalysis = new BaseModFAnalysisIncremental(b, ci_di_wi) with LIFOWorklistAlgorithm[SchemeExp]
    def fifoAnalysis(b: SchemeExp): IncrementalAnalysis = new BaseModFAnalysisIncremental(b, ci_di_wi) with FIFOWorklistAlgorithm[SchemeExp]
    def randAnalysis(b: SchemeExp): IncrementalAnalysis = new BaseModFAnalysisIncremental(b, ci_di_wi) with RandomWorklistAlgorithm[SchemeExp]
end IncrementalModFCPWLIndependenceTests

class IncrementalModFCPWLIndependenceTestsWithWI extends IncrementalModFCPWLIndependenceTests:
    override def configurations: List[IncrementalConfiguration] = super.configurations.filter(_.writeInvalidation)
    
class IncrementalModFCPWLIndependenceTestsWithoutWI extends IncrementalModFCPWLIndependenceTests:
    override def configurations: List[IncrementalConfiguration] = super.configurations.filterNot(_.writeInvalidation)
