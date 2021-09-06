package maf.language.ContractScheme
import maf.language.scheme._

case object ContractSchemeLexicalAddresser extends BaseSchemeLexicalAddresser:
    override def translate(exp: SchemeExp, lenv: LexicalEnv): SchemeExp = exp match
        case ContractSchemeDepContract(domains, rangeMaker, idn) =>
          ContractSchemeDepContract(
            domains.map(d => translate(d, lenv)),
            translate(rangeMaker, lenv),
            idn,
          )

        case ContractSchemeFlatContract(expr, idn) =>
          ContractSchemeFlatContract(translate(expr, lenv), idn)

        case ContractSchemeMon(contract, expr, idn) =>
          ContractSchemeMon(
            translate(contract, lenv),
            translate(expr, lenv),
            idn
          )

        case _ => super.translate(exp, lenv)
