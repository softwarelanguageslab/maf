package maf.cli.experiments.clients

import maf.modular.scheme.modf.SimpleSchemeModFAnalysis
import maf.util.{Reader, Writer}
import maf.core.Identity
import maf.util.benchmarks.Table
import maf.modular.ModAnalysis
import maf.language.scheme.*
import maf.modular.scheme.modf.SchemeModFSemanticsM
import maf.modular.scheme.modf.BigStepModFSemantics
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.worklist.FIFOWorklistAlgorithm
import scala.reflect.ClassTag
import maf.language.scheme.lattices.SchemeLattice
import maf.core.Address
import maf.cli.modular.scv.JVMSatSolver
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.benchmarks.Timeout
import maf.language.ContractScheme.ContractSchemeParser
import java.util.concurrent.TimeoutException

/** A dead code detection analysis, is a client analysis of the ModF analysis */
trait DeadcodeDetection extends BigStepModFSemanticsT:
    var visitedIdn: Set[Identity] = Set()

    override def intraAnalysis(cmp: Component): DeadcodeDetectionIntra

    trait DeadcodeDetectionIntra extends IntraAnalysis with BigStepModFIntraT:
        override def eval(exp: SchemeExp): EvalM[Value] =
            visitedIdn = visitedIdn + exp.idn
            super.eval(exp)

object DeadcodeDetection:
    type Analysis = DeadcodeDetection

    /**
     * Creates a dead code detection analysis
     *
     * @param program
     *   the program to analyze
     */
    def createAnalysis(program: SchemeExp): DeadcodeDetection =
      new ModAnalysis[SchemeExp](program)
        with StandardSchemeModFComponents
        with SchemeModFSemanticsM
        with SchemeModFNoSensitivity
        with BigStepModFSemantics
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp]
        with DeadcodeDetection:

          class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with DeadcodeDetectionIntra with BigStepModFIntra
          override def intraAnalysis(cmp: Component): AnalysisIntra =
            new AnalysisIntra(cmp)

    /**
     * Creates a dead code detection analysis for SCV programs
     *
     * @param program
     *   the program to analyze
     */
    def createScvAnalysis(program: SchemeExp): DeadcodeDetection =
        import maf.modular.scv.ScvSymbolicStore.given
        import maf.modular.scv.*
        new ModAnalysis(program)
          with ScvBigStepSemantics
          with ScvBigStepWithProvides
          with ScvWithStructs
          with SchemeConstantPropagationDomain
          with StandardSchemeModFComponents
          with FIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFSemanticsM
          with ScvOneContextSensitivity
          with DeadcodeDetection:
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp) with IntraScvSemanticsWithProvides with IntraScvSemanticsWithStructs with DeadcodeDetectionIntra

            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver

    /** Parses the given program text to a SchemeExp */
    def parseProgram(txt: String): SchemeExp =
      ContractSchemeParser.parse(txt)

    /**
     * Returns a set of expressions that was not visited during the analysis.
     *
     * These expressions are, by definition dead code as the analysis will always visit paths that are possibly reachable during execution due to its
     * sound overapproximations
     */
    def run(mkAnalysis: SchemeExp => DeadcodeDetection)(program: String): Set[Identity] =
        import scala.concurrent.duration.*
        val exp = parseProgram(program)
        val analysis = mkAnalysis(exp)
        try analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        catch case _: TimeoutException => ()

        exp.allSubexpressions.map(_.idn).toSet -- analysis.visitedIdn

/** Runs the dead code analysis using the SCV analyser on the Nguyen scv benchmark */
object DeadCodeScvBenchmarks:
    val benchmarks: List[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks.toList

    def main(args: Array[String]): Unit =
        val results = benchmarks.map(Reader.loadFile andThen DeadcodeDetection.run(DeadcodeDetection.createScvAnalysis))
        val outputTable = results.zip(benchmarks).foldLeft(Table.empty[Int]) { case (table, (result, benchmark)) =>
          table.add(benchmark, "# dead lines", result.map(_.pos.line).size)
        }
        val writer = Writer.openTimeStamped("out/dead-code-scv.csv")
        writer.write(outputTable.toCSVString(rowName = "benchmark"))
        writer.close()
