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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

trait BenchmarkPartition(part: Int, parts: Int) extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] =
        val all = super.benchmarks
        val partSize: Int = Math.ceil(all.size / parts).toInt
        val lower = (part - 1) * partSize
        val upper = part * partSize
        all.toList.sorted.slice(lower, upper).toSet

// The above parameters don't work (initialisation order issue), so workaround using duplication for now.
trait Part_1_3 extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] =
        val all = super.benchmarks
        val partSize: Int = Math.ceil(all.size / 3).toInt
        val lower = 0
        val upper = partSize
        all.toList.sorted.slice(lower, upper).toSet

trait Part_2_3 extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] =
        val all = super.benchmarks
        val partSize: Int = Math.ceil(all.size / 3).toInt
        val lower = partSize
        val upper = 2 * partSize
        all.toList.sorted.slice(lower, upper).toSet

trait Part_3_3 extends SchemeBenchmarkTests:
    override def benchmarks: Set[Benchmark] =
        val all = super.benchmarks
        val partSize: Int = Math.ceil(all.size / 3).toInt
        val lower = 2 * partSize
        val upper = all.size
        all.toList.sorted.slice(lower, upper).toSet
