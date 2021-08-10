package maf.modular.scheme.modflocal

import maf.modular._
import maf.modular.scheme._
import maf.language.scheme._
import maf.core._

trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp] {
  this: SchemeModFLocalSensitivity =>

  var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

  override protected def extendV(sto: Store[Adr, Storable], adr: Adr, vlu: Val): sto.This = {
    adr match {
      case _: VarAddr[_] | _: PtrAddr[_] =>
        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
      case _ => ()
    }
    super.extendV(sto, adr, vlu)
  }

  override protected def updateV(sto: Store[Adr, Storable], adr: Adr, vlu: Val): sto.This = {
    adr match {
      case _: VarAddr[_] | _: PtrAddr[_] =>
        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
      case _ => ()
    }
    super.updateV(sto, adr, vlu)
  }
}
