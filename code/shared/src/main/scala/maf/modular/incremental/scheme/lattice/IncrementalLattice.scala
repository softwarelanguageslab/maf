package maf.modular.incremental.scheme.lattice

import maf.core.{Address, Lattice}

/**
 * A lattice with annotations.
 * @tparam AL
 *   The actual lattice, i.e., the type of abstract values.
 * @tparam A
 *   The type of annotations, in our case, addresses.
 */
trait IncrementalLattice[AL, A <: Address] extends Lattice[AL]:
    def addAddress(v: AL, address: A): AL = addAddresses(v, Set(address))
    def addAddresses(v: AL, addresses: Set[A]): AL
    def getAddresses(v: AL): Set[A]
    def removeAddresses(v: AL): AL

    /** Shows the differences between x and y (from the perspective of a modular lattice). */
    def compare(x: AL, y: AL, xname: String = "x", yname: String = "y"): String
