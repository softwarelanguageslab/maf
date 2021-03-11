package maf.cli.runnables

import maf.cli.experiments.contracts.{Benchmarks, NguyenBenchmarks}
import maf.cli.experiments.contracts.MafBenchmarks

object SoftContractVerification {
  def main(args: Array[String]): Unit = {
    //val benchmarks: List[Benchmarks] = List(NguyenBenchmarks(), MafBenchmarks())
    val benchmarks = List(MafBenchmarks())
    benchmarks.foreach(_.run())
  }
}
