package maf.lattice.interfaces.taint

import maf.core.Lattice

/** A lattice for taints. The lattice is parameterised by the actual type of taints. */
trait TaintLattice[Taint, T[_]] extends Lattice[T[Taint]] { // Parameterise T itself to allow more explicit typing (though makes stuff more complex).
  def inject(t: Taint): T[Taint]
}
