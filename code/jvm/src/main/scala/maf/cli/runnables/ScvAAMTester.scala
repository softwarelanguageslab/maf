package maf.cli.runnables

import maf.core.*
import maf.cli.modular.scv.*
import maf.aam.scv.*
import maf.aam.{AAMAnalysis, SimpleWorklistSystem}
import maf.aam.scheme.*
import maf.language.scheme.*
import maf.modular.scheme.*
import maf.modular.scv.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.ContractScheme.*

object ScvAAMTester extends AAMTesterT:
    type Analysis = ScvAAMSemantics

    protected def analysis(b: SchemeExp): Analysis =
      new ScvAAMSemantics(b)
        with BaseSchemeAAMSemantics
        with AAMAnalysis[SchemeExp]
        with SchemeAAMContextInsensitivity
        with SchemeConstantPropagationDomain
        //with SchemeStoreAllocateReturn
        with SchemeFunctionCallBoundary
        with SchemeAAMLocalStore
        with SimpleWorklistSystem[SchemeExp]
        with SchemeAAMAnalysisResults {
        lazy val satSolver: ScvSatSolver[LatVal] =
            given lat: SchemeLattice[LatVal, Address] = lattice
            new JVMSatSolver
      }

    override protected def parseProgram(txt: String): SchemeExp =
      SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)

    def main(args: Array[String]): Unit =
      if args.size > 0 then run(args(0)) else println("Please provide a file")
