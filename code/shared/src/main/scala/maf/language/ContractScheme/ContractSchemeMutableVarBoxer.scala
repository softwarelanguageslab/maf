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

        case _ => super.rewrite(exp, mut, rew)
