package maf.language.ContractScheme.interpreter

object ContractSchemeErrors:
    case class ContractSchemeTypeError(msg: String) extends Exception
