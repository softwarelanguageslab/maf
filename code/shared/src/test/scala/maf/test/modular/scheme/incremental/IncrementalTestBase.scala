package maf.test.modular.scheme.incremental

import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration.allConfigurations
import maf.modular.incremental.{IncrementalConfiguration, IncrementalGlobalStore, IncrementalModAnalysis}
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.{GlobalStore, ModAnalysis, ReturnValue}
import maf.test.{IncrementalTest, SchemeBenchmarkTests, SlowTest}
import maf.util.benchmarks.Timeout
import org.scalatest.Tag

import scala.concurrent.duration.{Duration, MINUTES}

trait IncrementalTestBase extends SchemeBenchmarkTests {

    def name(): String

    def testTags(b: Benchmark): Seq[Tag] = Seq(IncrementalTest, SlowTest)

    type IncrementalAnalysis = IncrementalModAnalysis[SchemeExp]
        with IncrementalGlobalStore[SchemeExp]
        with ReturnValue[SchemeExp]
        with SchemeDomain
        with StandardSchemeModFComponents

    def analysis(b: SchemeExp): IncrementalAnalysis

    def analysisTimeout(): Timeout.T = Timeout.start(Duration(3, MINUTES))

    def configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

    def checkEqState(a: IncrementalAnalysis, b: IncrementalAnalysis, message: String): Unit =
        assert(a.store == b.store, message + " (store mismatch)")
        assert(a.visited == b.visited, message + " (visited set mismatch)")
        assert(a.deps == b.deps, message + " (dependency mismatch)")
        assert(a.mapping == b.mapping, message + " (mapping mismatch)")
        assert(a.cachedReadDeps == b.cachedReadDeps, message + " (read deps mismatch)")
        assert(a.cachedSpawns == b.cachedSpawns, message + " (spawns mismatch)")
        assert(a.provenance == b.provenance, message + " (provenance mismatch)")
        assert(a.cachedWrites == b.cachedWrites, message + " (write cache mismatch)")
        //assert(a.implicitFlows == b.implicitFlows, message + " (flow mismatch)") // TODO Readd?
        assert(a.dataFlowR == b.dataFlowR, message + " (reverse flow mismatch)")
}
