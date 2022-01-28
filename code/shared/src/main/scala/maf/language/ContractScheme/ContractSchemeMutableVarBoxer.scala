package maf.language.ContractScheme

import maf.language.scheme.SchemeMutableVarBoxer

import maf.core._
import maf.language.sexp._
import maf.language.scheme.*
import maf.language.ContractScheme.*
import scala.util.parsing.combinator.lexical.Lexical

object ContractSchemeMutableVarBoxer extends BaseSchemeMutableVarBoxer:

    override def rewrite(exp: SchemeExp, mut: Set[LexicalRef], rew: Rewrites): SchemeExp = exp match
        case ContractSchemeMon(contract, expression, idn) =>
          ContractSchemeMon(rewrite(contract, mut, rew), rewrite(expression, mut, rew), idn)

        case ContractSchemeFlatContract(flat, idn) =>
          ContractSchemeFlatContract(rewrite(flat, mut, rew), idn)

        case ContractSchemeDepContract(domains, rangeMaker, idn) =>
          ContractSchemeDepContract(domains.map(rewrite(_, mut, rew)), rewrite(rangeMaker, mut, rew), idn)

        case ContractSchemeProvide(outs, idn) =>
          ContractSchemeProvide(outs.map { case ContractSchemeContractOut(name, contract, idn) =>
                                  ContractSchemeContractOut(name, rewrite(contract, mut, rew), idn)
                                },
                                idn
          )

        case ContractSchemeCheck(contract, valueExpression, idn) =>
          ContractSchemeCheck(rewrite(contract, mut, rew), rewrite(valueExpression, mut, rew), idn)

        case m: MakeStruct => m // no variables to box here

        case _ => super.rewrite(exp, mut, rew)
