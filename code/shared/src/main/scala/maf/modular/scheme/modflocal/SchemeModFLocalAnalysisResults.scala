package maf.modular.scheme.modflocal

import maf.core._
import maf.modular._
import maf.modular.scheme._
import maf.language.scheme._

trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

    override def extendV(lcl: Sto, adr: Adr, vlu: Val)(using sto: StoreOps[Sto, Adr, Val]): lcl.Delta =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
              resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
            case _ => ()
        super.extendV(lcl, adr, vlu)

    override def updateV(lcl: Sto, adr: Adr, vlu: Val)(using sto: StoreOps[Sto, Adr, Val]): lcl.Delta =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
              resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
            case _ => ()
        super.updateV(lcl, adr, vlu)
