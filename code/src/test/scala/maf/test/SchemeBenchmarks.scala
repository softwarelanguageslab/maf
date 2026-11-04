package maf.test

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.datastructures.SmartUnion

trait VariousSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.various)
    
trait AllSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.sequentialBenchmarks)

