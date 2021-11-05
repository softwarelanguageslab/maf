package maf.test

import maf.bench.scheme.{ContractBenchmarkPrograms, IncrementalSchemeBenchmarkPrograms, SchemeBenchmarkPrograms}
import maf.util.datastructures.SmartUnion

trait VariousSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.various)

trait RandomSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.selectRandomSeq(40))

trait AllSequentialBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.sequentialBenchmarks)

trait ThreadBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.threads)

trait AllConcurrentBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.concurrentBenchmarks)

trait AllBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.allBenchmarks)

/** Benchmarks from the JSS 2021 paper titled: A parallel worklist algorithm and its exploration heuristics for static modular analyses */
trait JSS2021Benchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, SchemeBenchmarkPrograms.jss2021)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

trait ConcurrentIncrementalBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, IncrementalSchemeBenchmarkPrograms.threads)

trait SequentialIncrementalBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, IncrementalSchemeBenchmarkPrograms.sequential)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

trait ContractBenchmarks extends maf.test.SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, ContractBenchmarkPrograms.allBenchmarks)

trait ContractSafetyTestsBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, ContractBenchmarkPrograms.manualSafe)

trait ContractSoundnessTestsBenchmarks extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] = SmartUnion.sunion(super.benchmarks, ContractBenchmarkPrograms.manualUnsafe)
