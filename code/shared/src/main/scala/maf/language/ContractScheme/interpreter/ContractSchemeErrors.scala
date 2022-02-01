package maf.language.ContractScheme.interpreter

import maf.core.Identity as Idn

object ContractSchemeErrors:
    case class ContractSchemeTypeError(msg: String) extends Exception
    case class ContractSchemeBlame(lcontract: Idn, lserver: Idn, monIdn: Option[Idn]) extends Exception
    case class ContractSchemeInvalidArity(got: Int, expected: Int) extends Exception
