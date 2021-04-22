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
 *   {a1}  {a2} {a3}  {a4}  ... 
 *     \\    \    |    /    //
 *               {}
 */
// format: on
object AddressTaintLattice {

  type TaintSet = Set[Address]
  sealed trait T[Taint]
  case class Taints(taints: TaintSet) extends T[TaintSet]

  class AddressTaintLattice extends TaintLattice[TaintSet, T] {
    override def inject(t: TaintSet): T[TaintSet] = Taints(t)
    override def bottom: T[TaintSet] = Taints(Set())
    override def join(x: T[TaintSet], y: => T[TaintSet]): T[TaintSet] = (x, y) match { case (Taints(x), Taints(y)) => Taints(x.union(y)) }
    override def subsumes(x: T[TaintSet], y: => T[TaintSet]): Boolean = (x, y) match { case (Taints(x), Taints(y)) => y.subsetOf(x) }
    override def eql[B: BoolLattice](x: T[TaintSet], y: T[TaintSet]): B = (x, y) match {
      case (Taints(x), Taints(y)) => BoolLattice[B].inject(x == y)
    }
    override def show(v: T[TaintSet]): String = v.toString
  }
}
