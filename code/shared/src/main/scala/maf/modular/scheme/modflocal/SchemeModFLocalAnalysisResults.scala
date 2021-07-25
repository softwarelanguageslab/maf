package maf.modular.scheme.modflocal

import maf.modular._
import maf.language.scheme._
import maf.core._

trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp] {
  this: SchemeModFLocalSensitivity =>

  var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

  override protected def extendV(sto: Store[Adr, Storable], adr: Adr, vlu: Val): sto.This = {
    resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
    super.extendV(sto, adr, vlu)
  }

  override protected def updateV(sto: Store[Adr, Storable], adr: Adr, vlu: Val): sto.This = {
    resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
    super.updateV(sto, adr, vlu)
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
