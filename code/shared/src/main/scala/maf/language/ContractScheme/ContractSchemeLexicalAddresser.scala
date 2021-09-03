package maf.language.ContractScheme
import maf.language.scheme._

case object ContractSchemeLexicalAddresser extends BaseSchemeLexicalAddresser {
    override def translate(exp: SchemeExp, scope: Scope): SchemeExp = exp match {
      case ContractSchemeDepContract(domains, rangeMaker, idn) => 
        ContractSchemeDepContract(
          domains.map(d => translate(d, scope)),
          translate(rangeMaker, scope),
          idn,
        )

      case ContractSchemeFlatContract(expr, idn) => 
        ContractSchemeFlatContract(translate(expr, scope), idn)

      case ContractSchemeMon(contract, expr, idn) => 
        ContractSchemeMon(
          translate(contract, scope), 
          translate(expr, scope),
          idn
        )

      case _ => super.translate(exp, scope)
    }

}

