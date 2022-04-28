package maf.cli.experiments.scv

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.modular.scheme.*
import maf.modular.scv.*
import maf.modular.worklist.*
import scala.reflect.ClassTag
import maf.util.Reader
import maf.language.ContractScheme.*
import maf.modular.scheme.modf.*
import maf.language.symbolic.lattices.*
import maf.cli.modular.scv.*
import maf.language.scheme.lattices.*
import maf.modular.*
import maf.core.{Address, Identity, Monad}
import maf.language.scheme.*
import maf.modular.scv.RemovePathCondition

/** Keeps track of the components that where widened using the Nguyen widening strategy */
object TrackWidenedComponents:
    trait TrackComponentWidening extends RemovePathCondition with ComponentContentSimilarity:
        var widenedComponents: List[KPathCondition[Value]] = List()

        override def widen[M[_]: Monad](cmps: List[KPathCondition[Value]], fresh: M[SchemeExp], clo: (SchemeLambdaExp, Env)): M[KPathCondition[Value]] =
            val currentCtx = cmps.last
            widenedComponents = currentCtx :: widenedComponents
            super.widen(cmps, fresh, clo)

    def analysis(prg: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(prg)
            with ScvBigStepSemantics
            with SymbolicSchemeConstantPropagationDomain
            with StandardSchemeModFComponents
            with LIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFSemanticsM
            with ScvOneContextSensitivity(0)
            with ScvBigStepWithProvides
            with ScvWithStructs
            with ScvFullPathSensitivity
            with ScvIgnoreFreshBlame
            with UnstableWideningWithMinimum(2)
            with TrackComponentWidening:
            protected val valueClassTag: ClassTag[Value] = summon[ClassTag[Value]]

            override def intraAnalysis(
                cmp: Component
              ) = new IntraScvSemantics(cmp)
                with IntraScvSemanticsWithProvides
                with IntraScvSemanticsWithStructs
                with ScvFullPathSensitivityIntra
                with IntraScvIgnoreFreshBlames
                with IntraWidening

            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Address] = lattice
                new JVMSatSolver(this)

    def parseProgram(name: String): SchemeExp =
        val txt = Reader.loadFile(name)
        val parsed = ContractSchemeParser.compile(txt)
        val prelud = ContractSchemePrelude.addPrelude(List(parsed), incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = ContractSchemeMutableVarBoxer.transform(prelud)
        ContractSchemeUndefiner.undefine(transf)

    def benchmarks: List[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks.toList

    def main(args: Array[String]): Unit =
        val result: Map[String, List[Set[SchemeExp]]] = benchmarks
            .map(benchmark =>
                println(s"———————running benchmark: $benchmark")
                val program = parseProgram(benchmark)
                println(s"$program")
                val anl = analysis(program)
                try
                    anl.analyze()
                    (benchmark -> anl.widenedComponents.map(_.stoCache.values.toSet))
                catch case _ => (benchmark -> List())
            )
            .toMap

        result.foreach { case (k, v) =>
            v.zip(0 to v.size).foreach { case (vp, i) => println(s"$k,$i, $vp") }
        }
