package maf.cli.experiments.scv

import maf.cli.experiments.interpreter.*
import maf.util.Writer
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
        val benchmarks = SchemeBenchmarkPrograms.scvNguyenBenchmarks
        val coverage = benchmarks.map(runCoverage)
        val table =
          coverage.zip(benchmarks).foldLeft(Table.empty[Double]) { case (table, (coverage, benchmark)) => table.add(benchmark, "coverage", coverage) }
        val writer = Writer.openTimeStamped("out/scv-line-coverage.csv")
        writer.write(table.toCSVString())
        writer.close()
