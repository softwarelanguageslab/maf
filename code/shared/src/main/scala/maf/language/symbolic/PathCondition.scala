package maf.language.symbolic

import maf.language.scheme.*
import maf.core.Monad.{MonadIterableOps, MonadSyntaxOps}
import maf.core.{Identifier, Identity, IdentityMonad, Monad}
import maf.util.{Monoid, MonoidInstances}
import maf.util.MonoidImplicits.FoldMapExtension

object PathCondition:
    def empty: PathCondition = PathCondition(EmptyFormula)
    def onlyVarsAllowed(symbolic: SchemeExp): Boolean =
      symbolic match
          case SchemeVar(_) | SchemeVarLex(_, _) | SchemeValue(_, _) => true
          case _                                                     => false

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
                         changes = SymReplace(vr, SchemeVar(Identifier(newName, idn))) :: changes
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

    /** Find the "lowest" identifier */
    def lowest: Int =
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

        if currentLowest == Int.MaxValue then 0 else currentLowest

    /** Apply the given list of changes (for example renames of symbols) */
    def revert(changeset: List[SymChange]): PathCondition =
      PathCondition(changeset.foldLeft(formula)((formula, change) => change.revert(formula)))

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
    def simplify[M[_]: Monad](oldRoots: Set[SchemeExp], allowed: SchemeExp => Boolean, fresh: M[SchemeExp]): M[(PathCondition, List[SymChange])] =
        var changes: List[SymChange] = List()
        var rewrites: Map[SchemeExp, SchemeExp] = Map()
        var roots: Set[SchemeExp] = oldRoots

        // only keep those assertions that are in the set of roots
        val garbageCollectedFormula =
          PathCondition(
            formula
              .mapOption(expr => if expr.allSubexpressions.collect { case e: SchemeExp => e }.exists(roots.contains) then Some(expr) else None)
              .getOrElse(EmptyFormula)
          )

        for
            // rewrite the formula such that only allowed roots are represented by their original expression
            rewrittenFormula <- garbageCollectedFormula.formula
              .mapOptionM(expr =>
                mapExpr[M](expr) { subexpr =>
                  // if the sub expression is in the roots, and it is allowed, it can be kepts as is
                  if roots.contains(subexpr) && allowed(subexpr) then Monad[M].unit(Some(subexpr))
                  // if it is in the roots, but it is not allowed, it needs to be rewritten
                  else if roots.contains(subexpr) then
                      for
                          nww <- rewrites.get(subexpr).map(Monad[M].unit).getOrElse(fresh)
                          _ = { rewrites = rewrites + (subexpr -> nww) }
                          _ = { roots = roots + nww }
                          _ = { changes = SymReplace(subexpr, nww) :: changes }
                      yield Some(nww)
                  // if it is not in the roots it is kept to be cleaned up in the next pass
                  else Monad[M].unit(None)
                }.map(Some(_))
              )
              .map(formula => PathCondition(formula.getOrElse(EmptyFormula)))

            // then re-index
            (reindexPathCondition, additionalChanges) = rewrittenFormula.reindex(rewrittenFormula.lowest)
        yield (PathCondition(DNF.dnf(reindexPathCondition.formula)), changes ++ additionalChanges)

    /** Extend the path condition with the given assertion */
    def extend(assertion: SchemeExp): PathCondition =
        import FormulaAux.*
        this.copy(formula = DNF.dnf(conj(ass(assertion), formula)))

    def revert(change: SymChange): PathCondition =
      this.copy(formula = change.apply(formula))
