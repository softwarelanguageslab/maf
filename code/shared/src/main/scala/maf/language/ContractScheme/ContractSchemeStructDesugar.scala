package maf.language.ContractScheme

import maf.language.scheme.*

/**
 * Desugars `struct` declarations.
 *
 * `Structs` can only be placed in locations where `define`s are allowed.
 *
 * @see
 *   SchemeUndefiner.scala
 */
object ContractSchemeStructDesugar:
    def desugar(exps: List[SchemeExp]): List[SchemeExp] = ???
