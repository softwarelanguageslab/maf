package maf.language.ContractScheme

/** Soft-contract verification concrete internal values */

import maf.core.Identity

object ContractValues {

  /**
   * A blame represents two (possibly distinct) source locations.
   *
   * The `blamedPosition` represents the location of the code that violated a particular contract, while `blamingPosition` represents the location of
   * the contract that is being violated.
   */
  case class Blame(blamedPosition: Identity, blamingPosition: Identity)

  /**
   * A guard which represents the value of a dependent contract after evaluation. <code> (~> domain rangeMaker) </code>
   *
   * @tparam L
   *   the type of abstract value contained within the contract value
   */
  case class Grd[L](domain: List[L], rangeMaker: L) {
    def map[AL](f: L => AL): Grd[AL] = Grd(domain.map(f), f(rangeMaker))
  }

  /**
   * A monitor on a dependent contract (mon (~> domain rangeMaker)/lcontract procedure/lserver)
   *
   * @tparam L
   *   the type of abstract value contained within the contract value
   */
  case class Arr[L](
      lcontract: Identity,
      lserver: Identity,
      contract: L,
      e: L,
      topLevel: Boolean = false) {
    def map[AL](f: L => AL): Arr[AL] = Arr(lcontract, lserver, f(contract), f(e), topLevel)
  }

  /**
   * A value that represents a flat contract, such that we can distribute blames correctly when a value of this type is applied.
   *
   * @tparam L
   *   the type of abstract value contained within the contract value
   */
  case class Flat[L](contract: L) {
    def map[AL](f: L => AL): Flat[AL] = Flat(f(contract))
  }
}
