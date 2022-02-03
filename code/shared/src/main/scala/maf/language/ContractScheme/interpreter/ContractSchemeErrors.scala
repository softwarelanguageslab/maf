package maf.language.ContractScheme.interpreter

import maf.core.Identity as Idn

object ContractSchemeErrors:
    trait ContractSchemeError extends Exception
    case class ContractSchemeTypeError(msg: String) extends ContractSchemeError
    case class ContractSchemeBlame(lcontract: Idn, lserver: Idn, monIdn: Option[Idn]) extends ContractSchemeError
    case class ContractSchemeInvalidArity(got: Int, expected: Int) extends ContractSchemeError
    case class ContractSchemeNoMatchClauseMatched() extends ContractSchemeError
