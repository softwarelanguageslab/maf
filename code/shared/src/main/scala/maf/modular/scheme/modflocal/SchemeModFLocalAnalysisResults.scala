package maf.modular.scheme.modflocal

import maf.modular._
import maf.modular.scheme._
import maf.language.scheme._

trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp] {
  this: SchemeModFLocalSensitivity with SchemeDomain =>

  var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

  override def extendV(sto: Sto, adr: Adr, vlu: Val): Sto = {
    adr match {
      case _: VarAddr[_] | _: PtrAddr[_] =>
        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
      case _ => ()
    }
    super.extendV(sto, adr, vlu)
  }

  override def updateV(sto: Sto, adr: Adr, vlu: Val): Sto = {
    adr match {
      case _: VarAddr[_] | _: PtrAddr[_] =>
        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
      case _ => ()
    }
    super.updateV(sto, adr, vlu)
  }
}
