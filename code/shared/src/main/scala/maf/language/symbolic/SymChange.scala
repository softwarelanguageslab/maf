package maf.language.symbolic

import maf.language.scheme.*
import maf.core.Monad

sealed trait SymChange:
    /** Apply the change described by a sublass of SymChange */
    def apply(old: Formula): Formula

    /** Revert the change */
    def revert(old: Formula): Formula

case object NoChange extends SymChange:
    def apply(old: Formula): Formula = old
    def revert(old: Formula): Formula = old

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
