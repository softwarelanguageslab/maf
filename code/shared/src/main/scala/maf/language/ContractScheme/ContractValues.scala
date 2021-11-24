package maf.language.ContractScheme

/** Soft-contract verification concrete internal values */

import maf.core.Identity
import maf.language.scheme._

object ContractValues:

    /**
     * A blame represents two (possibly distinct) source locations.
     *
     * The `blamedPosition` represents the location of the code that violated a particular contract, while `blamingPosition` represents the location
     * of the contract that is being violated.
     */
    case class Blame(blamedPosition: Identity, blamingPosition: Identity)

    /**
     * A guard which represents the value of a dependent contract after evaluation. <code> (~> domain rangeMaker) </code>
     *
     * @tparam L
     *   the type of abstract value contained within the contract value
     */
    case class Grd[L](domain: List[L], rangeMaker: L, domainIdns: List[Identity], rangeMakerExpr: SchemeExp):
        def map[AL](f: L => AL): Grd[AL] = Grd(domain.map(f), f(rangeMaker), domainIdns, rangeMakerExpr)

    /**
     * A monitor on a dependent contract (mon (~> domain rangeMaker)/lcontract procedure/lserver)
     *
     * @tparam L
     *   the type of abstract value contained within the contract value
     */
    case class Arr[L](
        lcontract: Identity,
        lserver: Identity,
        contract: Grd[L],
        e: L,
        topLevel: Boolean = false):
        def map[AL](f: L => AL): Arr[AL] = Arr(lcontract, lserver, contract.map(f), f(e), topLevel)
        def checkArgs[A](l: List[A]): Boolean =
          contract.domain.size == l.size
        def expectedNumArgs: Int = contract.domain.size

    /**
     * A value that represents a flat contract, such that we can distribute blames correctly when a value of this type is applied.
     *
     * @param contract
     *   the wrapped contract
     * @param fexp
     *   the original expression AST node from (flat expr)
     * @param contractIdn
     *   the location in the source code where the flat contract orginated from
     * @param sym
     *   the symbolic representation of the flat contract (at definition time) if available
     * @tparam L
     *   the type of abstract value contained within the contract value
     */
    case class Flat[L](contract: L, fexp: SchemeExp, sym: Option[SchemeExp], contractIdn: Identity):
        def map[AL](f: L => AL): Flat[AL] = Flat(f(contract), fexp, sym, contractIdn)

    /**
     * A representation for an apaque value, opaque values are used as substitutes for any value and have the same semantics as a "top" value in a
     * lattice.
     *
     * The value is used instead of top because (1) it makes it explicit that it is coming from SCV and is not a result of some over-approximation (2)
     * allows for a "top" value to exist in any abstract domain, which is not possible in for example a (non-bounded) constant propagation lattice
     */
    case class Opq()
