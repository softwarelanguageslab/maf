package maf.language.ContractScheme

import maf.language.scheme._
import maf.core.{Identifier, Identity}

/**
 * Same as BaseSchemeUndefiner since all types of expressions supported by MAF are included there.
 *
 * This can be changed in the future to factor out those expressions that are not required by extending from BaseSchemeMonadicUndefiner
 */
object ContractSchemeUndefiner extends BaseSchemeUndefiner
