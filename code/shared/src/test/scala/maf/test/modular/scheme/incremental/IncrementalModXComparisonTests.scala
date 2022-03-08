package maf.test.modular.scheme.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.lattice.IncrementalSchemeConstantPropagationDomain
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf.*
import maf.modular.scheme.ssmodconc.*
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test.*
import maf.util.Reader
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import org.scalatest.Tag

import scala.concurrent.duration.*

/**
 * Compares the analysis result of an incremental analysis w.r.t. the results obtained by full program analyses. Requires equality for the analysis of
 * the initial program version, and subsumption for the analysis of the updated program version.
 */
// TODO: Require equality when all optimisations are enabled.
trait IncrementalModXComparisonTests extends SchemeBenchmarkTests:
    lazy val configurations: List[IncrementalConfiguration] = allConfigurations
    def timeout(): Timeout.T = Timeout.start(Duration(150, SECONDS))

    def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag]

    type Analysis <: ModAnalysis[SchemeExp]
    type IncrementalAnalysis <: IncrementalModAnalysis[SchemeExp]

    // The original program also needs a program version, as otherwise the programs of the analyses differ and components and some values cannot be compared.
    def analysis(e: SchemeExp, version: Version): Analysis
    def incAnalysis(e: SchemeExp): IncrementalAnalysis

    def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit
    def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit

    def onBenchmark(benchmark: Benchmark): Unit =
        val text = Reader.loadFile(benchmark)
        val program = CSchemeParser.parseProgram(text)
        for c <- configurations do
            property(s"Analysis results for $benchmark using $c are equal (initial analysis) or subsuming (incremental update).",
                     testTags(benchmark, c): _*
            ) {
              try
                  val a = analysis(program, Old)
                  a.analyzeWithTimeout(timeout())
                  assume(a.finished)
                  val i = incAnalysis(program)
                  i.configuration = c
                  i.analyzeWithTimeout(timeout())
                  assume(i.finished)
                  checkEqual(a, i)

                  ////

                  val b = analysis(program, New)
                  b.analyzeWithTimeout(timeout())
                  assume(b.finished)
                  i.updateAnalysis(timeout())
                  assume(i.finished)
                  checkSubsumption(b, i)

              catch
                  case e: VirtualMachineError =>
                    System.gc()
                    cancel(s"Analysis of $benchmark encountered an error: $e.")
              // case InvalidConfigurationException(msg, config)
              //   info(s"Analysis of $benchmark cannot be run using $config: invalid configuration encountered.")
            }

class ModFComparisonTests extends IncrementalModXComparisonTests:

    override def benchmarks: Set[Benchmark] =
      SmartUnion.sunion(SmartUnion.sunion(super.benchmarks, IncrementalSchemeBenchmarkPrograms.sequential),
                        IncrementalSchemeBenchmarkPrograms.sequentialGenerated
      )

    abstract class BaseAnalysis(program: SchemeExp)
        extends ModAnalysis[SchemeExp](program)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsM
        with IncrementalSchemeConstantPropagationDomain // Use the incremental domain for both.
        with LIFOWorklistAlgorithm[SchemeExp]

    class IncrementalAnalysis(program: SchemeExp)
        extends BaseAnalysis(program)
        with IncrementalSchemeModFBigStepSemantics
        with IncrementalGlobalStore[SchemeExp]:
        var configuration: IncrementalConfiguration = allOptimisations
        override def intraAnalysis(
            cmp: Component
          ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

    class Analysis(program: SchemeExp, version: Version) extends BaseAnalysis(program) with BigStepModFSemantics with GlobalStore[SchemeExp]:
        override def intraAnalysis(
            cmp: Component
          ) = new IntraAnalysis(cmp) with BigStepModFIntra with GlobalStoreIntra {
          override protected def eval(exp: SchemeExp): EvalM[Value] = exp match
              case SchemeCodeChange(e, _, _) if version == Old => eval(e)
              case SchemeCodeChange(_, e, _) if version == New => eval(e)
              case _                                           => super.eval(exp)
        }

    def analysis(e: SchemeExp, version: Version) = new Analysis(e, version)
    def incAnalysis(e: SchemeExp) = new IncrementalAnalysis(e)

    // Are slow: multiple-dwelling (both) and peval. All other can locally be completed with a timeout of 10 seconds.
    def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag] = Seq(IncrementalTest, SlowTest)

    def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit =
        // Check components.
        assert(i.visited == a.visited, "The visited sets of both analyses differ for the initial program version.")

        // Check dependencies.
        val depsI = i.deps.toSet[(Dependency, Set[i.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        val depsA = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        assert(depsI == depsA, "Both analyses have not inferred the same dependencies for the initial program version.")

        // Check store.
        assert(i.store.toSet == a.store.toSet, "Both analyses contain different value/address pairs in their store for the initial program version.")

    def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit =
        // Check components.
        assert(i.visited.size >= a.visited.size, "The incremental analysis did not visit at least the same components than the full reanalysis.")
        assert(a.visited.diff(i.visited).isEmpty, "The visited set of the incremental update does not subsume the visited set of the full reanalysis.") // If the size is equal, this checks also the converse assertion.

        // Check dependencies.
        val depsI = i.deps.toSet[(Dependency, Set[i.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        val depsA = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        assert(depsI.size >= depsA.size, "The incremental analysis did not infer at least the same dependencies than the full reanalysis.")
        assert(depsA.diff(depsI).isEmpty, "The dependency set of the incremental update does not subsume the dependency set of the full reanalysis.")

        // Check store.
        assert(i.store.size >= a.store.size, "The incrementally updated store is smaller than the store after a full reanalysis.")
        a.store.foreach { case (addr, av) =>
          val iv = i.store.getOrElse(addr, i.lattice.bottom)
          assert(a.lattice.subsumes(iv.asInstanceOf[a.Value], av), s"Store mismatch at $addr: $av is not subsumed by $iv.")
        }

class ModConcComparisonTests extends IncrementalModXComparisonTests with ConcurrentIncrementalBenchmarks:

    override lazy val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

    abstract class BaseModConcAnalysis(prg: SchemeExp)
        extends ModAnalysis[SchemeExp](prg)
        with KKallocModConc
        with LIFOWorklistAlgorithm[SchemeExp]
        with IncrementalSchemeConstantPropagationDomain:
        val k: Int = 1

    class IncrementalAnalysis(program: SchemeExp)
        extends BaseModConcAnalysis(program)
        with IncrementalSchemeModConcSmallStepSemantics
        with IncrementalGlobalStore[SchemeExp]:
        var configuration: IncrementalConfiguration = noOptimisations // allOptimisations

        override def intraAnalysis(
            cmp: Component
          ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis

    class Analysis(program: SchemeExp, version: Version)
        extends BaseModConcAnalysis(program)
        with SmallStepModConcSemantics
        with GlobalStore[SchemeExp]:
        override def intraAnalysis(
            cmp: SmallStepModConcComponent
          ): KCFAIntra = new IntraAnalysis(cmp) with SmallStepIntra with KCFAIntra with GlobalStoreIntra {
          override protected def evaluate(
              exp: Exp,
              env: Env,
              stack: Stack
            ): Set[State] = exp match
              case SchemeCodeChange(e, _, _) if version == Old => Set(Eval(e, env, stack))
              case SchemeCodeChange(_, e, _) if version == New => Set(Eval(e, env, stack))
              case _                                           => super.evaluate(exp, env, stack)
        }

    // Are slow: SICP-compiler, msort, actors. All other can locally be completed with a timeout of 10 seconds.
    def testTags(b: Benchmark, c: IncrementalConfiguration): Seq[Tag] = Seq(IncrementalTest, SlowTest)

    def analysis(e: SchemeExp, version: Version) = new Analysis(e, version)
    def incAnalysis(e: SchemeExp) = new IncrementalAnalysis(e)

    def checkEqual(a: Analysis, i: IncrementalAnalysis): Unit =
        // Check components.
        assert(i.visited == a.visited, "The visited sets of both analyses differ for the initial program version.")

        // Check dependencies.
        val depsI = i.deps.toSet[(Dependency, Set[i.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        val depsA = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        assert(depsI == depsA, "Both analyses have not inferred the same dependencies for the initial program version.")

        // Check store.
        assert(i.store.toSet == a.store.toSet, "Both analyses contain different value/address pairs in their store for the initial program version.")

    def checkSubsumption(a: Analysis, i: IncrementalAnalysis): Unit =
        // Check components.
        assert(i.visited.size >= a.visited.size, "The incremental analysis did not visit at least the same components than the full reanalysis.")
        assert(a.visited.diff(i.visited).isEmpty, "The visited set of the incremental update does not subsume the visited set of the full reanalysis.") // If the size is equal, this checks also the converse assertion.

        // Check dependencies.
        val depsI = i.deps.toSet[(Dependency, Set[i.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        val depsA = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        assert(depsI.size >= depsA.size, "The incremental analysis did not infer at least the same dependencies than the full reanalysis.")
        assert(depsA.diff(depsI).isEmpty, "The dependency set of the incremental update does not subsume the dependency set of the full reanalysis.")

        // Check store.
        assert(i.store.size >= a.store.size, "The incrementally updated store is smaller than the store after a full reanalysis.")
        a.store.foreach { case (addr, av) =>
          val iv = i.store.getOrElse(addr, i.lattice.bottom)
          assert(a.lattice.subsumes(iv.asInstanceOf[a.Value], av), s"Store mismatch at $addr: $av is not subsumed by $iv.")
        }
