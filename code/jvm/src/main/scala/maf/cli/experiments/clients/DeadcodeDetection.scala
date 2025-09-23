package maf.cli.experiments.clients

import maf.language.symbolic.lattices.*
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
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.benchmarks.Timeout
import java.util.concurrent.TimeoutException
import maf.util.MAFLogger

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
            with SymbolicSchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with DeadcodeDetection:

            class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with DeadcodeDetectionIntra with BigStepModFIntra
            override def intraAnalysis(cmp: Component): AnalysisIntra =
                new AnalysisIntra(cmp)

    /** Parses the given program text to a SchemeExp */
    def parseProgram(txt: String): SchemeExp =
        SchemeParser.parseProgram(txt)

    /**
     * Returns a set of expressions that was not visited during the analysis.
     *
     * These expressions are, by definition dead code as the analysis will always visit paths that are possibly reachable during execution due to its
     * sound overapproximations
     */
    def run(mkAnalysis: SchemeExp => DeadcodeDetection)(program: String): DeadCodeAnalysisResult =
        import scala.concurrent.duration.*
        val exp = parseProgram(program)
        val analysis = mkAnalysis(exp)
        try analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        catch case _ => ()

        val deadIdns = exp.allSubexpressions.map(_.idn).toSet -- analysis.visitedIdn

        DeadCodeAnalysisResult(
          deadIdns = deadIdns,
          fractionDeadLines = deadIdns.map(_.idn).size.toDouble / exp.allSubexpressions.map(_.idn).size.toDouble
        )

case class DeadCodeAnalysisResult(deadIdns: Set[Identity], fractionDeadLines: Double)

abstract class DeadcodeMain:
    val benchmarks: List[String]

    private def logFile(name: String): String =
        println(s"Analysing $name")
        name

    def run(name: String, analysis: SchemeExp => DeadcodeDetection): Unit =
        val results = benchmarks.map(logFile andThen Reader.loadFile andThen DeadcodeDetection.run(analysis))
        val outputTable = results.zip(benchmarks).foldLeft(Table.empty[Double]) { case (table, (result, benchmark)) =>
            table.add(benchmark, "% dead expressions", result.fractionDeadLines)
        }
        val writer = Writer.openTimeStamped(s"out/$name")
        Writer.write(writer, outputTable.toCSVString(rowName = "benchmark"))
        Writer.close(writer)

object DeadcodeSchemeBenchmarks extends DeadcodeMain:
    val benchmarks: List[String] =
        (SchemeBenchmarkPrograms.sequentialBenchmarks.filterNot(p =>
            // undefiner issues (define in invalid context) TODO: check if this is the case or it is an error in the undefiner
            p.startsWith("test/R5RS/ad") || p.startsWith("test/R5RS/WeiChenRompf2019/the-little-schemer")
        ) -- Set(
          // also undefiner issues
          "test/R5RS/various/lambda-update.scm",
          "test/R5RS/scp1/car-counter.scm",
          "test/R5RS/scp1/twitter.scm",
          "test/R5RS/scp1/university.scm",
          "test/R5RS/various/strong-update.scm", // not sure what's wrong here? thought we fixed that.
        )).toList

    def main(args: Array[String]): Unit =
        // progamatically disable the logger
        MAFLogger.disable()
        run("dead-code-scheme.csv", DeadcodeDetection.createAnalysis)