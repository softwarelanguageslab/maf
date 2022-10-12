package maf.lattice

import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice

object TaintLattice:
    /**
     * A simple taint lattice.
     *
     *      May Be Tainted
     *            |
     *         Tainted
     *            |
     *        Untainted
     *
    */
    sealed trait TL
    case object Untainted extends TL
    case object Tainted extends TL
    case object MayBeTainted extends TL

    class TaintLattice extends Lattice[TL]:
        val bottom: TL = Untainted
        override def isBottom(x: TL): Boolean = x == Untainted
        val top: TL = MayBeTainted

        def join(x: TL, y: => TL): TL = (x, y) match {
            case (Untainted, _) => y
            case (_, Untainted) => x
            case (MayBeTainted, _) => x
            case (_, MayBeTainted) => y
            case (Tainted, Tainted) => x
            case _ => MayBeTainted
        }

        def subsumes(x: TL, y: => TL): Boolean = x match {
            case MayBeTainted => true
            case Tainted => y != MayBeTainted
            case _ => y == Untainted
        }

        // TODO: check implementation
        def eql[B: BoolLattice](x: TL, y: TL): B = (x, y) match {
            case (Untainted, Untainted) => BoolLattice[B].inject(true)
            case (Tainted, Tainted) => BoolLattice[B].inject(true)
            case (MayBeTainted, _) => BoolLattice[B].top
            case (_, MayBeTainted) => BoolLattice[B].top
            case _ => BoolLattice[B].inject(false)
        }

        override def show(v: TL): String = v.toString

    implicit val taintLattice: TaintLattice = new TaintLattice