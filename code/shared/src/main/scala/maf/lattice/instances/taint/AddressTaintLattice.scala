package maf.lattice.instances.taint

import maf.core.Address
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.taint.TaintLattice

// format: off
/**
 * Set lattice for taints with addresses.
 * 
 *    {a1, a2}         ...
 *     /    \     
 *   {a1} {a2} {a3}  {a4}  ... 
 *     \    \    |    /    /
 *               {}
 */
// format: on
object AddressTaintLattice {

  type TaintSet = Set[Address]
  case class T[Taintset](taints: TaintSet) // We omit a separate trait so that we don't have to match in the definitions of join, subsumes and eql.

  class AddressTaintLattice extends TaintLattice[TaintSet, T] {
    override def inject(t: TaintSet): T[TaintSet] = T(t)
    override def bottom: T[TaintSet] = T(Set())
    override def join(x: T[TaintSet], y: => T[TaintSet]): T[TaintSet] = T(x.taints.union(y.taints))
    override def subsumes(x: T[TaintSet], y: => T[TaintSet]): Boolean = y.taints.subsetOf(x.taints)
    def eql[B: BoolLattice](x: T[TaintSet], y: T[TaintSet]): B = BoolLattice[B].inject(x.taints == y.taints)
    override def show(v: T[TaintSet]): String = v.toString
  }
}
