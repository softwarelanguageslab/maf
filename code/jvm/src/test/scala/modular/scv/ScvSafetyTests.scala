package maf.test.modular.scv

import maf.cli.modular.scv._
import maf.test._
import maf.language.scheme._
import maf.language.ContractScheme._
import maf.language.scheme._
import maf.language.scheme.lattices._
import maf.modular._
import maf.modular.scv._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.util._

/**
 * This type of test checks whether the analysis is able to obtain the expected precision.
 *
 * Precision in this case is measured whether it is able to elliminate program paths that introduce blames but that are actually infeasible at runtime
 */
trait ScvSafetyTests extends SchemeBenchmarkTests:
    protected def analysis(program: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(program)
          with ScvBigStepSemantics
          with SchemeConstantPropagationDomain
          with StandardSchemeModFComponents
          with LIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFSemantics
          with ScvOneContextSensitivity:
            override def intraAnalysis(cmp: Component) = new IntraScvSemantics(cmp)
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Addr] = lattice
                new JVMSatSolver()

    protected def parse(program: String): SchemeExp =
      ContractSchemeParser.parse(program.nn)

    override def onBenchmark(b: Benchmark): Unit =
      property(s"$b should contain no contract violations") {
        val content = Reader.loadFile(b)
        val an = analysis(parse(content))
        an.analyze()
        assert(an.summary.blames.size == 0)
        assert(an.returnValue(an.initialComponent) != an.lattice.bottom)
      }

class SimpleSafetyTests extends ScvSafetyTests with ContractSafetyTestsBenchmarks
