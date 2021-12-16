package maf.aam.scheme

import maf.core.Address

trait SchemeAAMAnalysisResults extends BaseSchemeAAMSemantics with maf.aam.AnalysisResults:
    var resultsPerIdn = Map().withDefaultValue(Set.empty[LatVal])

    abstract override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
        (addr, value) match
            case (_: VarAddr, Storable.V(value)) =>
              resultsPerIdn += addr.idn -> (resultsPerIdn(addr.idn) + value)
            case _ => ()
        super.writeSto(sto, addr, value)
