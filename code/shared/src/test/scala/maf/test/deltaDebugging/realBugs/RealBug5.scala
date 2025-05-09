package maf.test.deltaDebugging.realBugs

import maf.language.scheme.SchemeExp
import maf.modular.scheme.{SchemeDomain, VarAddr, PtrAddr}
import maf.modular.scheme.modflocal.{SchemeModFLocalAnalysisResults, SchemeModFLocalSensitivity}
import maf.modular.worklist.{LIFOWorklistAlgorithm, SequentialWorklistAlgorithm}
import maf.util.benchmarks.Timeout

trait RealBug5 extends SchemeModFLocalAnalysisResults:
  this: SchemeModFLocalSensitivity with SchemeDomain =>

  override def updateV(sto: Sto, adr: Adr, vlu: Val) =
    adr match
      case _: VarAddr[_] | _: PtrAddr[_] =>
        adr.idn -> (resultsPerIdn(adr.idn) + vlu) //store mismanaged here
      case _ => ()
    ??? //Delta.emptyDelta //return value wrong here
