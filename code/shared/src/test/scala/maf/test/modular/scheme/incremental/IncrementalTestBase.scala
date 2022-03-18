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

  def eqState(a: IncrementalAnalysis, b: IncrementalAnalysis): Boolean =
      a.store == b.store &&
      a.visited == b.visited &&
      a.deps == b.deps && 
      a.mapping == b.mapping &&
      a.cachedReadDeps == b.cachedReadDeps &&
      a.cachedSpawns == b.cachedSpawns &&
      a.provenance == b.provenance &&
      a.cachedWrites == b.cachedWrites &&
      a.implicitFlows == b.implicitFlows &&
      a.dataFlowR == b.dataFlowR
}
