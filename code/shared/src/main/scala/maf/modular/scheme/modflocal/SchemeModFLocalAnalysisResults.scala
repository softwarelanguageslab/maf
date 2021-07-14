package maf.modular.scheme.modflocal

import maf.modular._
import maf.language.scheme._

trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp] {
  
    var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)
    
    override protected def extendV(sto: Sto, adr: Adr, vlu: Val): Sto = {
        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
        super.extendV(sto, adr, vlu)
    }

    override def spawn(cmp: Component) = {
        cmp match {
            case HaltComponent(vlu, _) => 
                val idn = program.idn
                resultsPerIdn += idn -> (resultsPerIdn(idn) + vlu)
            case _ => ()
        }
        super.spawn(cmp)
    }
}
