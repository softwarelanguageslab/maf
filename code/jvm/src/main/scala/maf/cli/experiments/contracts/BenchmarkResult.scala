package maf.cli.experiments.contracts

import maf.util.benchmarks.Table

/**
  * A class class that collects the results of a benchmark
  *
  * @param benchmarkName the name of the benchmarkName
  * @param elapsedTime how much time it took to execute the benchmark, in nanoseconds
  * @param numberOfChecks the number of contracts that were checked, keep in mind that this is not the amount that will be checked at runtime, but rather how many times the analysis has verified a contract
  * @param analysedComponents the number of times a component has been analysed
  * @param correct is true when the analysis is deemed to have given the correct result
  */
case class BenchmarkResult(
    benchmarkName: String,
    elapsedTime: Long,
    numberOfChecks: Int,
    analysedComponents: Int,
    verifiedContracts: Int,
    distinctContracts: Int,
    correct: Boolean = true
) {

  /**
    * Add the result to the given table.
    *
    * @param inTable the table to which the result should be added
    * @returns a new table containing the result
    */
  def addToTable(inTable: Table[String]): Table[String] = {
    var outTable = inTable
    outTable = outTable.add(this.benchmarkName, "elapsedTime", this.elapsedTime.toString)
    outTable = outTable.add(
      this.benchmarkName,
      "analysedComponents",
      this.analysedComponents.toString
    )

    outTable = outTable.add(this.benchmarkName, "numberOfChecks", this.numberOfChecks.toString)
    outTable = outTable.add(this.benchmarkName, "correct", this.correct.toString())
    outTable =
      outTable.add(this.benchmarkName, "verifiedContracts", this.verifiedContracts.toString)

    outTable =
      outTable.add(this.benchmarkName, "distinctContracts", this.distinctContracts.toString)

    outTable
  }
}
