package maf.lattice

import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice

object TaintLattice:
    /**
     * A simple taint lattice.
     *
     * May Be Tainted
     * |
     * | Untainted
     */
    sealed trait TL
    case object Untainted extends TL
    case object MayBeTainted extends TL

    class TaintLattice extends Lattice[TL]:
        def level(v: TL): Int = v match
            case Untainted    => 1
            case MayBeTainted => 2
        val bottom: TL = Untainted
        override def isBottom(x: TL): Boolean = x == Untainted
        val top: TL = MayBeTainted

        def join(x: TL, y: => TL): TL = x match {
            case Untainted => y
            case _         => x
        }

        def subsumes(x: TL, y: => TL): Boolean = x match {
            case MayBeTainted => true
            case _            => y == Untainted
        }

        // TODO: check implementation
        def eql[B: BoolLattice](x: TL, y: TL): B = (x, y) match {
            case (Untainted, Untainted)       => BoolLattice[B].inject(true)
            case (MayBeTainted, MayBeTainted) => BoolLattice[B].top
            case _                            => BoolLattice[B].inject(false)
        }

        override def show(v: TL): String = v.toString

    implicit val taintLattice: TaintLattice = new TaintLattice
