package maf.cli.experiments
package domain

import scala.util.Try
import maf.util.{Reader, Writer}
import maf.language.scheme.SchemeParser
import maf.lattice.HMapInstrument
import maf.bench.scheme.*
import maf.util.benchmarks.Table

trait DomainSizePrograms:
    import SchemeBenchmarkPrograms.*
    val programsByCategory = Map("ad" -> ad, "gabriel" -> gabriel, "gambit" -> gambit, "scp1" -> scp1, "toplas98" -> toplas98, "wcr2019" -> WCR2019)
    val programs = programsByCategory.flatMap(_._2).toList

object DomainSize extends DomainSizePrograms:

    type AnalysisResult = Map[Int, Int]

    private def printUsage(): Unit =
        val usage = """Usage: 
        |   programs          Writes the program names to an output file in `results` called `programs`
        |   benchmark         Runs the benchmarks and writes the output to a timestamped file called `output` 
        |                     in the output oflder 
        """.stripMargin

        print(usage)

    private def runAnalysis(programName: String): Option[AnalysisResult] =
        val source = Reader.loadFile(programName)
        val program = SchemeParser.parseProgram(source)
        val analysis = SchemeAnalyses.contextInsensitiveAnalysis(program)
        HMapInstrument.reset()
        Try(analysis.analyze()).toOption.map(_ => HMapInstrument.count)

    /**
     * Write the program names to an output file structured as an SCV. each row has a number of columns corresponding to the benchmark used in each
     * source. This can then be processed for counting purposes, SLOC, etc.
     */
    private def printProgramNames(): Unit =
        val w = Writer.open("results/programs.csv")
        val programs = programsByCategory.map { case (k, v) => k + "," + v.toList.mkString(",") }.mkString("\n")
        Writer.write(w, programs)
        Writer.close(w)

    private def runBenchmarks(): Unit =
        val results = programs.map(runAnalysis)
        val csv = programs
            .zip(results)
            .foldLeft(Table.empty.copy(default = Some(0))) { case (table, (program, result)) =>
                result
                    .map(_.foldLeft(table) { case (table, (size, count)) =>
                        table.add(program, size.toString, count)
                    })
                    .getOrElse(table)
            }
            .toCSVString()

        val w = Writer.open("results/output.csv")
        Writer.write(w, csv)
        Writer.close(w)

    def main(args: Array[String]): Unit =
        val action = Try(args(0)).toOption
        action match
            case Some("programs")  => printProgramNames()
            case Some("benchmark") => runBenchmarks()
            case _                 => printUsage()
