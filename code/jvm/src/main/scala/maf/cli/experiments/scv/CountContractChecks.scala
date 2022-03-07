package maf.cli.experiments.scv

import maf.language.scheme.*

/**
 * Statically count all contract checks.
 *
 * For primitives OpqOps is used to determine the number of contracts that are checked
 */
trait CountContractChecks:

    /**
     * A class holding the counts of the number of syntactic contract checks.
     *
     * @param implicitCounts
     *   the number of implicit contract checks (from primitives)
     * @param explicitCount
     *   the number of explicit contract checks (from user defined contracts)
     */
    case class ContractCheckCounts(implicitCount: Int, explicitCount: Int)

    def countChecks(expr: SchemeExp): ContractCheckCounts = ???
