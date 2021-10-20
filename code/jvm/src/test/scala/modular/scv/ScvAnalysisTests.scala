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

trait ScvAnalysisTests extends SchemeBenchmarkTests:
    protected type Analysis = ScvModAnalysis

    protected def analysis(program: SchemeExp): Analysis =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(program)
          with ScvBigStepSemantics
          with SchemeConstantPropagationDomain
          with StandardSchemeModFComponents
          with LIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFSemanticsM
          with ScvOneContextSensitivity:
            override def intraAnalysis(cmp: Component) = new IntraScvSemantics(cmp)
            override val sat: ScvSatSolver[Value] =
                given SchemeLattice[Value, Addr] = lattice
                new JVMSatSolver()

    protected def parse(program: String): SchemeExp =
      ContractSchemeParser.parse(program.nn)

    protected def runFromFile(b: Benchmark)(onAnalysisDone: (Analysis) => Unit): Unit =
        val content = Reader.loadFile(b)
        val an = analysis(parse(content))
        an.analyze()
        onAnalysisDone(an)
