package maf.language.symbolic

import maf.language.scheme.*
import maf.core.Monad
import Symbolic.*

object SymChange:
    def lowestOpt(syms: List[SymChange]): Option[Int] =
        val lowests = syms.flatMap(change => change.constituents).flatMap(v => SymbolicStore.lowestOpt(SymbolicStore.variables(v)))
        if lowests.size > 0 then Some(lowests.min) else None

    def applyAll(changes: List[SymChange], symbolic: Symbolic): Symbolic =
        changes.foldLeft(symbolic)((s, change) => change.apply(Assertion(s)).asInstanceOf[Assertion].contents)

sealed trait SymChange:
    /** Apply the change described by a sublass of SymChange */
    def apply(old: Formula): Formula

    /** Revert the change */
    def revert(old: Formula): Formula

    /** Returns the constituents of the symbolic change */
    def constituents: List[Symbolic]

    /** Apply a change on the change */
    def applyChanges(change: List[SymChange]): SymChange

    def vars: List[String] = this.constituents.flatMap(Symbolic.variables)

    def apply(e: Symbolic): Symbolic = this.apply(Assertion(e)).asInstanceOf[Assertion].contents

case object NoChange extends SymChange:
    def apply(old: Formula): Formula = old
    def revert(old: Formula): Formula = old
    def constituents: List[Symbolic] = List()
    def applyChanges(changes: List[SymChange]): SymChange = NoChange

case class SymReplace(from: SchemeExp, to: SchemeExp) extends SymChange:
    def apply(old: SchemeExp, revert: Boolean): SchemeExp =
        def visit(e: SchemeExp): SchemeExp =
            if !revert && e == from then to
            else if revert && e == to then from
            else
                (e match
                    case SchemeFuncall(f, args, idn) =>
                        SchemeFuncall(visit(f), args.map(visit), idn)
                    case SchemeVar(_) | SchemeVarLex(_, _) | SchemeValue(_, _) => e
                    case _                                                     => throw new Exception(s"unsupported symbolic expression $e")
            )

        visit(old)

    def apply(old: Formula): Formula = old.map(apply(_, false))
    def revert(old: Formula): Formula = old.map(apply(_, true))
    def constituents: List[Symbolic] = List(from, to)
    def applyChanges(changes: List[SymChange]): SymChange =
        val fromChanged = changes.foldLeft(this.from)((c, change) => change.apply(c))
        val toChanged = changes.foldLeft(this.to)((c, change) => change.apply(c))
        SymReplace(fromChanged, toChanged)
