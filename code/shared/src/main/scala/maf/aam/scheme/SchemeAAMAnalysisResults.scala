package maf.aam.scheme

import maf.core.Address
import maf.modular.AnalysisResults
import maf.language.scheme.*

trait SchemeAAMAnalysisResults extends BaseSchemeAAMSemantics with AnalysisResults[SchemeExp]:
    var resultsPerIdn = Map().withDefaultValue(Set.empty[LatVal])

    abstract override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
        (addr, value) match
            case (_: VarAddr, Storable.V(value)) =>
              resultsPerIdn += addr.idn -> (resultsPerIdn(addr.idn) + value)
            case _ => ()
        super.writeSto(sto, addr, value)
