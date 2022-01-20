package maf.language.CScheme

import maf.language.scheme._

/**
 * Same as BaseSchemeUndefiner since all types of expressions supported by MAF are included there.
 *
 * This can be changed in the future to factor out those expressions that are not required by extending from BaseSchemeMonadicUndefiner
 */
object CSchemeUndefiner extends BaseSchemeUndefiner
