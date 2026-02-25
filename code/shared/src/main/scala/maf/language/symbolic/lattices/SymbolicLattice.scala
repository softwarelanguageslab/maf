package maf.language.symbolic.lattices

import maf.language.symbolic.{Var => _, *}
import maf.lattice.interfaces.BoolLattice
import maf.core.{Lattice, LatticeTopUndefined}

import Symbolic.*

trait SymbolicWidenPolicy:
    def widen(v: Symbolic, rest: Set[Symbolic]): Symbolic

/**
 * Widens a symbolic representation if it contains recurring patterns.
 *
 * For example:
 *
 * Given rest: (+ x 1), v: (+ (+ x 1) 1)
 *
 * Then v is transformed such that: (+ □ 1)
 *
 * where □ is a hole that must be filled later on
 */
case object WidenRecurringPatterns extends SymbolicWidenPolicy:
    /** Recursively decend the given symbolic representation, and replace all the recurring expressions with a hole */
    private def visit(v: Symbolic, recurring: Set[Symbolic]): Symbolic =
        v match
            case Funcall(fexp, fargs, idn) =>
                val fexp1 = if recurring.contains(fexp) then `□` else visit(fexp, recurring)
                val fargs1 = fargs.map(farg => if recurring.contains(farg) then `□` else visit(farg, recurring))
                Funcall(fexp1, fargs1, idn)
            case vlu @ Value(_, idn) =>
                if recurring.contains(vlu) then `□` else vlu
            case vrr @ Var(_) =>
                if recurring.contains(vrr) then `□` else vrr
            case _ => throw new Exception("not a valid symbolic representation")

    def widen(v: Symbolic, rest: Set[Symbolic]): Symbolic =
        val recurring = v.allSubexpressions
            .collect { case f @ Funcall(_, _, _) =>
                f
            }
            .filter(e => rest.exists(_.isomorphic(e)))

        visit(v, recurring.toSet)

class WidenNesting(maxDeep: Int, maxWide: Int) extends SymbolicWidenPolicy:
    private def visit(v: Symbolic, wide: Int, deep: Int): Symbolic =
        v match
            case Funcall(fexp, fargs, idn) if wide <= maxWide && deep <= maxDeep =>
                val replacedFexp = visit(fexp, wide, deep + 1)
                val replacedFargs = fargs.foldRight((List[Symbolic](), 0)) { case (farg, (fargs, wideness)) =>
                    (visit(farg, wideness, deep + 1) :: fargs, wideness + 1)
                }
                Funcall(replacedFexp, replacedFargs._1, idn)

            case Var(_) | Value(_, _) => v
    //case _                    => Hole()

    def widen(v: Symbolic, rest: Set[Symbolic]): Symbolic = visit(v, 0, 0)

object SymbolicLattice:
    type L = Set[Symbolic]

/**
 * A lattice for representing symbolic representations of values.
 *
 * It is implemented as a powerset lattice over scheme expressions.
 *
 * The join and subsumes relations are sound, but the lattice is infinite in height and width. Therefore a widening operator is necessary when it is
 * used in an actual analysis. Such a widening operation is implemented in the `widen` function
 */
class SymbolicLattice[B: BoolLattice] extends Lattice[Set[Symbolic]]:
    def level(v: Set[Symbolic]) = v.size
    type L = Set[Symbolic]
    def bottom: L = Set()
    override def isBottom(x: L): Boolean = x.isEmpty
    def top: L = throw LatticeTopUndefined
    def join(x: L, y: => L): L =
        (x ++ y)

    def subsumes(x: L, y: => L): Boolean =
        y subsetOf x

    def eql[B: BoolLattice](x: L, y: L): B = BoolLattice[B].inject(x == y)
    def show(v: L): String = s"{${v.mkString(",")}}"

    // Disable widening for now
    def widen(p: SymbolicWidenPolicy)(v: L): L = v /* v.map(e => p.widen(e, v.filterNot(e == _))) */
