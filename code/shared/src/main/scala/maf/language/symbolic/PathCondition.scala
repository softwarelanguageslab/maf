package maf.language.symbolic

import maf.language.scheme.*
import maf.language.symbolic.Symbolic.*
import maf.language.symbolic.Symbolic
import maf.core.Monad.{MonadIterableOps, MonadSyntaxOps}
import maf.core.{Identifier, Identity, IdentityMonad, Monad}
import maf.core.MonadOptionT.given

import maf.core.MonadOptionT
import maf.core.MonadOptionT.OptionT
import maf.util.{Monoid, MonoidInstances}
import maf.util.MonoidImplicits.FoldMapExtension

object PathCondition:
    def empty: PathCondition = PathCondition(EmptyFormula)
    def onlyVarsAllowed(symbolic: SchemeExp, level: Int): Boolean =
        symbolic match
            case SchemeFuncall(_, _, _) if level == 0                     => true
            case SchemeFuncall(Identifier(("true?" | "false?"), _), _, _) => true
            case SchemeVar(_) | SchemeVarLex(_, _) | SchemeValue(_, _)    => true
            case _                                                        => false

    def visit[M[_]: Monad, T: Monoid](expr: SchemeExp)(f: SchemeExp => M[Option[T]]): M[T] = expr match
        case funcall @ SchemeFuncall(fexp, args, _) =>
            f(funcall).flatMap {
                case Some(v) => Monad[M].unit(v)
                case None =>
                    for
                        ffexp <- f(fexp)
                        fargs <- args.mapM(f(_)).map(_.flatten)
                        visitedF <- ffexp.map(Monad[M].unit).getOrElse(visit(fexp)(f))
                        visitedArgs <- if fargs.size == args.size then Monad[M].unit(fargs) else args.mapM(visit(_)(f))
                    yield Monoid[T].append(visitedF, visitedArgs.foldMap(x => x))
            }
        case vrr @ SchemeVar(_)       => f(vrr).map(_.getOrElse(Monoid[T].zero))
        case vrr @ SchemeVarLex(_, _) => f(vrr).map(_.getOrElse(Monoid[T].zero))
        case vll @ SchemeValue(_, _)  => f(vll).map(_.getOrElse(Monoid[T].zero))

case class PathCondition(formula: Formula):
    import PathCondition.*

    /** Alpha renaming of the path condition */
    def reindexed: (PathCondition, List[SymChange]) =
        this.reindex(this.lowest)

    /**
     * Rename/reset the names of the identifiers based on the first available variable number.
     *
     * For example if we have in the path condition: (number? x3) /\ (x3 = 0) then this can be renamed to (number? x0) /\ (x0 = 0) without changing
     * its meaning.
     *
     * This is a heavy operation, to be used sparingly.
     */
    def reindex(lowest: Int): (PathCondition, List[SymChange]) =
        var changes: List[SymChange] = List()
        val newFormula = formula.map(expr =>
            mapExpr[IdentityMonad.Id](expr) {
                // only vars are interesting to rewrite
                case vr @ SchemeVar(Identifier(name, idn)) =>
                    Some(
                      SchemeVar(
                        Identifier(
                          // identifiers other than xYY do not introduce a change
                          (if name.startsWith("x") then
                               val newName = s"x${name.split('x')(1).toInt - lowest}"
                               changes = if newName != name then SymReplace(vr, SchemeVar(Identifier(newName, idn))) :: changes else changes
                               newName
                           else name)
                          ,
                          idn
                        )
                      )
                    )
                // anything else stays the same
                case _ => None
            }
        )

        (PathCondition(newFormula), changes.reverse)

    def mapExpr[M[_]: Monad](expr: SchemeExp)(f: SchemeExp => M[Option[SchemeExp]]): M[SchemeExp] = expr match
        case funcall @ SchemeFuncall(fexp, args, idn) =>
            f(funcall).flatMap {
                case Some(v) => Monad[M].unit(v)
                case None =>
                    for
                        ffexp <- f(fexp)
                        fargs <- args.mapM(f(_)).map(_.flatten)
                        visitedF <- ffexp.map(Monad[M].unit).getOrElse(mapExpr(fexp)(f))
                        visitedArgs <- if fargs.size == args.size then Monad[M].unit(fargs) else args.mapM(mapExpr(_)(f))
                    yield SchemeFuncall(visitedF, visitedArgs, idn)
            }
        case vrr @ SchemeVar(_)       => f(vrr).map(_.getOrElse(vrr))
        case vrr @ SchemeVarLex(_, _) => f(vrr).map(_.getOrElse(vrr))
        case vll @ SchemeValue(_, _)  => f(vll).map(_.getOrElse(vll))

    private def variables(f: Formula): List[Int] = f match
        case Conjunction(cs) => cs.flatMap(variables).toList
        case Disjunction(ds) => ds.flatMap(variables).toList
        case Assertion(f)    => SymbolicStore.variables(f).map(_.split('x')(1).toInt)
        case EmptyFormula    => List()

    def highest: Int = variables(this.formula) match
        case xs @ (_ :: _) => xs.max
        case _             => 0

    /** Find the "lowest" identifier */
    def lowestOpt: Option[Int] = this.formula match
        case EmptyFormula => None
        case _ =>
            var currentLowest = Int.MaxValue
            formula.map { expr =>
                given minMonoid: Monoid[Int] = MonoidInstances.intMinMonoid
                val foundLowest = visit[IdentityMonad.Id, Int](expr) {
                    case SchemeVar(Identifier(name, _)) =>
                        if name.startsWith("x") then Some(name.split('x')(1).toInt) else None
                    case _ => None
                }
                if currentLowest > foundLowest then currentLowest = foundLowest
                expr
            }

            if currentLowest == Int.MaxValue then None else Some(currentLowest)

    def lowest: Int = lowestOpt.getOrElse(0)

    /** Apply the given list of changes (for example renames of symbols) */
    def revert(changeset: List[SymChange]): PathCondition =
        PathCondition(changeset.foldLeft(formula)((formula, change) => change.revert(formula)))

    /** Garbage collect the path condition. This notion is defined as "simplification" in the paper. */
    def gc(roots: Set[SchemeExp], maxLevel: Int = 5): PathCondition =
        import FormulaAux.*
        val candidates = roots
        def isRequired(exp: Symbolic, level: Int): Boolean =
            candidates.exists(e => Unstable.isomorphic(exp, e) && level < maxLevel) || (exp match
                case Funcall(fexp, fargs, _) =>
                    isRequired(fexp, level + 1) || fargs.exists(isRequired(_, level + 1))
                case _ => false
            )

        def visitFormula(formula: Formula): Formula = formula match
            case Conjunction(cs) =>
                conj(cs.toList.map(visitFormula): _*)
            case Disjunction(cs) =>
                disj(cs.toList.map(visitFormula): _*)
            case Assertion(exp) if isRequired(exp, 0) =>
                Assertion(exp)
            case Assertion(_) => EmptyFormula
            case EmptyFormula => EmptyFormula

        PathCondition(visitFormula(this.formula))

    /**
     * Simplify the current path expression, replacing assertions with simplified versions if required
     *
     * @param roots
     *   the symbolic representations to keep in the path condition
     * @param allowed
     *   a predicate that returns true if the expression must not be replaced with a fresh variable expression
     * @param fresh
     *   a monadic computation that returns a fresh identifier
     * @return
     *   a path condition where only the roots are available and potentially replaced
     */
    def simplify[M[_]: Monad](oldRoots: Set[SchemeExp], allowed: (SchemeExp, Int) => Boolean, fresh: M[SchemeExp]): M[(PathCondition, List[SymChange])] =

        var changes: List[SymChange] = List()
        var rewrites: Map[SchemeExp, SchemeExp] = Map()
        var roots: Set[SchemeExp] = oldRoots

        // only keep those assertions that are in the set of roots
        val garbageCollectedFormula = this.gc(roots)

        def visit(
            exp: SchemeExp,
            level: Int,
          ): OptionT[M, SchemeExp] = exp match
            case SchemeFuncall(SchemeVar(id @ Identifier(("true?" | "false?"), _)), args, idn) =>
                for fargsSimplified <- args.mapM(visit(_, level))
                yield SchemeFuncall(SchemeVar(id), fargsSimplified, idn)

            case SchemeFuncall(fexp, args, idn) if allowed(exp, level) =>
                for
                    fexpSimplified <- visit(fexp, level + 1)
                    argsSimplified <- args.mapM(visit(_, level + 1))
                yield SchemeFuncall(fexpSimplified, argsSimplified, idn)
            case SchemeVar(_) if allowed(exp, level)       => MonadOptionT.optionInstance.unit(exp)
            case SchemeVarLex(_, _) if allowed(exp, level) => MonadOptionT.optionInstance.unit(exp)
            case SchemeValue(_, _) if allowed(exp, level)  => MonadOptionT.optionInstance.unit(exp)
            // If none of the above is true, we will allocate a fresh variable
            case _ =>
                if exp.allSubexpressions.collect { case e: SchemeExp => e } exists (e =>
                        roots.flatMap(_.allSubexpressions).contains(e) || roots.contains(e)
                    )
                then
                    for
                        nww <- rewrites.get(exp).map(MonadOptionT.optionInstance.unit).getOrElse(MonadOptionT.lift(fresh))
                        _ = { rewrites = rewrites + (exp -> nww) }
                        _ = { changes = SymReplace(exp, nww) :: changes }
                    yield nww
                else OptionT(Monad[M].unit(None))

        for
            // rewrite the formula such that only allowed roots are represented by their original expression
            rewrittenFormula <- garbageCollectedFormula.formula
                .mapOptionM(expr => visit(expr, 0).inner)
                .map(formula => PathCondition(formula.getOrElse(EmptyFormula)))

            // re-compute the roots using the performed rewrites
            newRoots = changes.reverse
                .foldLeft(roots.toList)((roots, change) => roots.map(r => change.apply(Assertion(r))).map(_.asInstanceOf[Assertion].contents))
                .toSet

            rewrittenGcFormula = rewrittenFormula.gc(newRoots)

            // then re-index
            (reindexPathCondition, additionalChanges) = rewrittenGcFormula.reindex(rewrittenGcFormula.lowest)
        yield (PathCondition(DNF.dnf(reindexPathCondition.formula)), changes ++ additionalChanges)

    /** Extend the path condition with the given assertion */
    def extend(assertion: SchemeExp): PathCondition =
        import FormulaAux.*
        this.copy(formula = DNF.dnf(conj(ass(assertion), formula)))

    def applyChanges(changes: List[SymChange]): PathCondition =
        this.copy(formula = changes.foldLeft(formula)((formula, change) => change.apply(formula)))

    def revert(change: SymChange): PathCondition =
        this.copy(formula = change.revert(formula))

    def vars(f: Formula): List[String] = f match
        case Conjunction(cs) => cs.flatMap(vars).toList
        case Disjunction(ds) => ds.flatMap(vars).toList
        case Assertion(ass)  => Symbolic.variables(ass)
        case EmptyFormula    => List()

    def stripIdn: PathCondition = this.copy(formula = Symbolic.stripIdn(formula))

    def vars: List[String] = vars(this.formula)
