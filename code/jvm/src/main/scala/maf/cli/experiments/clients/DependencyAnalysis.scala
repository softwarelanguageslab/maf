package maf.cli.experiments.clients

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.modular.scv.*
import maf.modular.*
import maf.modular.worklist.*
import maf.modular.scheme.modf.*
import maf.modular.scv.*
import maf.modular.scheme.*
import maf.language.scheme.lattices.*
import maf.language.scheme.*
import maf.core.Address
import scala.reflect.ClassTag

/**
 * An analysis that gives additional information about the dependencies in the analysis.
 *
 * It measures:
 *
 * * The average (and total) number of call dependencies for each component (not including itself) * The average (and total) number of write
 * dependencies for each component (not including itself) * The average (and total) number of read dependencies for each component (not including
 * itself) * The average (and total) number of read and write dependencies to itself * The average (and total) number of calls to itself
 */
trait DependencyAnalysis extends ModAnalysis[SchemeExp]:
    /** Return the component associated with the given address */
    def viewComponent(adr: Dependency): Component = adr match
        case AddrDependency(addr) =>
            (addr match
                case ReturnAddr(cmp, _) => cmp
                case VarAddr(_, cmp)    => cmp
                case PtrAddr(_, cmp)    => cmp
                case _                  => throw new Exception(s"unsupported address $addr")
            ).asInstanceOf[Component] // UNSAFE

    /** Set of read dependencies from a given component to another component */
    var readDeps: Set[(Component, Component)] = Set()

    /** Set of write dependencies from a given component to another component */
    var writeDeps: Set[(Component, Component)] = Set()

    /** Set of calls from a component to another component */
    var calls: Set[(Component, Component)] = Set()

    override def intraAnalysis(component: Component): IntraDependencyAnalysis

    trait IntraDependencyAnalysis extends IntraAnalysis:
        override def register(dep: Dependency): Unit =
            readDeps = readDeps + (this.component -> viewComponent(dep))
            super.register(dep)

        override def trigger(dep: Dependency): Unit =
            writeDeps = writeDeps + (this.component -> viewComponent(dep))
            super.trigger(dep)

        override def spawn(cmp: Component): Unit =
            calls = calls + (this.component -> cmp)
            super.spawn(cmp)

trait DependencyAnalysisRunner extends ClientAnalysisRunner:
    override type Analysis <: DependencyAnalysis
    override type Result = Double

    def computeAverageAndTotal(anl: Analysis, results: Set[(anl.Component, anl.Component)]): (Double, Double) =
        val asMap = results.foldLeft(Map[anl.Component, List[anl.Component]]()) { case (result, (from, to)) =>
            result + (from -> (to :: result.get(from).getOrElse(List())))
        }
        val numberOfComponents = asMap.keys.size
        val total = asMap.values.map(_.size).sum
        (total / numberOfComponents, numberOfComponents)

    def results(analysis: Analysis): Map[String, Result] =
        val (writeNoSelfAvg, writeNoSelfTotal) = computeAverageAndTotal(analysis, analysis.writeDeps.filter(dep => dep._1 != dep._2))
        val (readNoSelfAvg, readNoSelfTotal) = computeAverageAndTotal(analysis, analysis.readDeps.filter(dep => dep._1 != dep._2))
        val (callNoSelfAvg, callNoSelfTotal) = computeAverageAndTotal(analysis, analysis.calls.filter(dep => dep._1 != dep._2))
        val (writeSelfAvg, writeSelfTotal) = computeAverageAndTotal(analysis, analysis.writeDeps.filter(dep => dep._1 == dep._2))
        val (readSelfAvg, readSelfTotal) = computeAverageAndTotal(analysis, analysis.readDeps.filter(dep => dep._1 == dep._2))
        val (callSelfAvg, callSelfTotal) = computeAverageAndTotal(analysis, analysis.calls.filter(dep => dep._1 == dep._2))

        Map(
          // no self
          "writeDepsAvgNoSelf" -> writeNoSelfAvg,
          "readDepsAvgNoSelf" -> readNoSelfAvg,
          "callDepsNoSelf" -> callNoSelfAvg,
          "writeNoSelfTotal" -> writeNoSelfTotal,
          "readNoSelfTotal" -> readNoSelfTotal,
          "callNoSelfTotal" -> callNoSelfTotal,
          // self
          "writeDepsAvgSelf" -> writeSelfAvg,
          "readDepsAvgSelf" -> readSelfAvg,
          "callDepsSelf" -> callSelfAvg,
          "writeSelfTotal" -> writeSelfTotal,
          "readSelfTotal" -> readSelfTotal,
          "callSelfTotal" -> callSelfTotal,
        )

trait ScvDependencyAnalysisRunner extends DependencyAnalysisRunner with ScvClientAnalysis:
    type Analysis = ScvDependencyAnalysis

    class ScvDependencyAnalysis(program: SchemeExp)
        extends ModAnalysis(program)
        with ScvBigStepSemantics
        with ScvBigStepWithProvides
        with ScvWithStructs
        with SchemeConstantPropagationDomain
        with StandardSchemeModFComponents
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFSemanticsM
        with ScvOneContextSensitivity(0)
        with DependencyAnalysis:
        protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

        override def intraAnalysis(
            cmp: Component
          ) = new IntraScvSemantics(cmp) with IntraScvSemanticsWithProvides with IntraScvSemanticsWithStructs with IntraDependencyAnalysis

        override val sat: ScvSatSolver[Value] =
            given SchemeLattice[Value, Address] = lattice
            new JVMSatSolver(this)

    def createAnalysis(exp: SchemeExp): Analysis = new ScvDependencyAnalysis(exp)

object NguyenScvDependencyAnalysisRunner extends ScvDependencyAnalysisRunner:
    def benchmarks: Set[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks
