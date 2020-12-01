package maf.cli.runnables

import maf.cli.experiments.contracts.{Benchmarks, NguyenBenchmarks}

object SoftContractVerification {
  def main(args: Array[String]): Unit = {
    val benchmarks: List[Benchmarks] = List(NguyenBenchmarks())
    benchmarks.foreach(_.run())
  }
}
