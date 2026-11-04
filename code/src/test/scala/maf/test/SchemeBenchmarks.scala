package maf.test

import maf.runners.SchemeBenchmarkPrograms
import maf.util.SmartUnion

trait VariousSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.various)
    
trait AllSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.sequentialBenchmarks)

