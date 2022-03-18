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

  val name: String

  def testTags(b: Benchmark): Seq[Tag] = Seq(IncrementalTest, SlowTest)

  type IncrementalAnalysis = IncrementalModAnalysis[SchemeExp]
    with IncrementalGlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SchemeDomain
    with StandardSchemeModFComponents

  def analysis(b: SchemeExp): IncrementalAnalysis

  def analysisTimeout(): Timeout.T = Timeout.start(Duration(3, MINUTES))

  val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

  def checkEqState(a: IncrementalAnalysis, b: IncrementalAnalysis, message: String): Unit =
      if a.store != b.store then fail(message + " (store mismatch)")
      if a.visited != b.visited then fail(message +  " (visited set mismatch)")
      if a.deps != b.deps then fail(message + " (dependency mismatch)")
      if a.mapping != b.mapping then fail(message + " (mapping mismatch)")
      if a.cachedReadDeps != b.cachedReadDeps then fail(message + " (read deps mismatch)")
      if a.cachedSpawns != b.cachedSpawns then fail(message + " (spawns mismatch)")
      if a.provenance != b.provenance then fail(message + " (provenance mismatch)")
      if a.cachedWrites != b.cachedWrites then fail(message + " (write cache mismatch)")
      if a.implicitFlows != b.implicitFlows then fail(message + " (flow mismatch)")
      if a.dataFlowR != b.dataFlowR then fail(message + " (reverse flow mismatch)")
}
