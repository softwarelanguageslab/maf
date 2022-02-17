package maf.cli.experiments.scv

import maf.cli.experiments.interpreter.*
import maf.util.{PythonBridge, Writer}
import maf.util.benchmarks.Table
import maf.bench.scheme.SchemeBenchmarkPrograms

/**
 * Tests what the coverage is providing the programs from the Nguyen Scv benchmarks with random values.
 *
 * Outputs a coverage.scv to out/
 */
object ScvInputCoverage:
    def runCoverage(file: String): Double =
        println(s"Running coverage on $file")
        CodeCoverage.scvLineCoverage(file)

    def main(args: Array[String]): Unit =
        val benchmarks = SchemeBenchmarkPrograms.scvNguyenBenchmarks.toList
        val coverage = benchmarks.map(runCoverage)
        val table =
          coverage.zip(benchmarks).foldLeft(Table.empty[Double]) { case (table, (coverage, benchmark)) => table.add(benchmark, "coverage", coverage) }
        val (writer, name) = Writer.openTimeStampedGetName("out/scv-line-coverage.csv")
        writer.write(table.toCSVString(rowName = "name"))
        writer.close()

        val python = PythonBridge("./scripts/Python/scv/")
        val basePath = System.getProperty("user.dir")
        if !python.runScript("linecoverage.py", s"$basePath/$name") then println("Unable to write output to visualisation")
        else println(s"successfully written visualisation to ${name}.pdf")
