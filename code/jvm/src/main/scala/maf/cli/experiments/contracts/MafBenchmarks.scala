package maf.cli.experiments.contracts

/**
 * This benchmark suite contains benchmark of the MAF framework,
 * but with contracts annotated in the source code of many functions.
 */
case class MafBenchmarks() extends Benchmarks {

  def run(): Unit = {
    val testBenchmarks = List(
      fromFile("test/soft-contract/mceval.scm")
      //fromFile("test/soft-contract/qeval.scm"),
      //fromFile("test/soft-contract/frogger.scm")
    )

    runAll(testBenchmarks, "test_maf_scv")
  }

}
