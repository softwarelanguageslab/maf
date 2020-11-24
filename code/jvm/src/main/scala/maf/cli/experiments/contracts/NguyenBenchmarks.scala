package maf.cli.experiments.contracts

object NguyenBenchmarks extends Benchmarks {
  val testBenchmarks = List(
    fromFile("tests/soft-contract/NguyenGTH18/safe/dynamic-tests.rkt")
  )

  runAll(testBenchmarks, "test_benchmarks_svc")
}
