package maf.cli.experiments.clients

import maf.language.scheme.SchemeExp
import maf.modular.{AnalysisEntry, ModAnalysis}
import maf.util.Reader
import maf.util.MAFLogger
import maf.util.benchmarks._
import maf.util._


trait ClientAnalysisRunner:
    import scala.concurrent.duration.*

    type Analysis <: AnalysisEntry[SchemeExp]
    type Result

    def timeout: Timeout.T = Timeout.start(1.minute)

    def benchmarks: Set[String]

    /** A function to create an analysis */
    def createAnalysis(exp: SchemeExp): Analysis

    /** Parses a string to a scheme expression */
    def parseProgram(program: String): SchemeExp

    /**
     * A map from program names to map of results.
     *
     * Each result is identifier by a name
     *
     * @param analysis
     *   the analysis to extract the results from
     */
    def results(analysis: Analysis): Map[String, Result]

    protected def writeToFile(output: String, path: String) = 
      val writer = Writer.open(path)
      Writer.write(writer, output)
      Writer.close(writer)


    protected def toTable(results: Map[String, Map[String, Result]], inTable: Table[Result] = Table.empty): Table[Result] =
        results.foldLeft(inTable) { case (table, (programName, metrics)) =>
            metrics.foldLeft(table) { case (table, (name, vlu)) => table.add(programName, name, vlu) }
        }

    protected def runBenchmark(table: Table[Result], name: String): Table[Result] =
        val program = Reader.loadFile(name)
        val expr = parseProgram(program)
        val analysis = createAnalysis(expr)
        analysis.analyzeWithTimeout(timeout)
        toTable(Map(name -> results(analysis)), table)

    def main(args: Array[String]): Unit =
        MAFLogger.disable()
        val table: Table[Result] = Table.empty
        val resultTable = benchmarks.foldLeft(table)(runBenchmark)
        println(resultTable.prettyString())
