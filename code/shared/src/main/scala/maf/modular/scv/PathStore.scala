package maf.modular.scv

import maf.language.scheme._
import maf.core.Identifier
import maf.core.Monad
import maf.core.Monad.MonadSyntaxOps
import maf.core.Monad.MonadIterableOps

/** A symbolic Scheme expression together with all of its identifiers */
case class Symbolic(expr: SchemeExp, identifiers: Set[String]):
    /* Reindex the expression on the given base */
    def reindex(base: Int): Symbolic =
        def visit(exp: SchemeExp): SchemeExp = exp match
            case SchemeFuncall(f, args, idn) =>
              SchemeFuncall(visit(f), args.map(visit), idn)
            case SchemeVar(id) =>
              SchemeVar(Identifier(Symbolic.reindex(base, id.name), id.idn))

            case SchemeVarLex(id, lexAdr) =>
              SchemeVarLex(Identifier(Symbolic.reindex(base, id.name), id.idn), lexAdr)
            case v @ SchemeValue(value, idn) => v
            case _                           => throw new Exception(s"unsupported symbolic expression $exp")

        Symbolic(visit(expr))

object Symbolic:
    /** Alternative constructor for `Symbolic`, computes the set of identifiers using the free variables from the expression */
    def apply(expr: SchemeExp): Symbolic =
      Symbolic(expr, expr.fv)

    def identifiers(syms: Iterable[SchemeExp]): Set[String] =
      syms.collect {
        case SchemeVar(id)       => id.name
        case SchemeVarLex(id, _) => id.name
      }.toSet

    object Implicits:
        /** Implicitly converts any SchemeExp to an instance of `Symbolic` */
        implicit def toSymbolic(exp: SchemeExp): Symbolic =
          Symbolic(exp)

    def reindex(lowest: Int, vr: String): String =
      if vr.startsWith("x") then s"x${vr.split('x')(1).toInt - lowest}"
      else vr

sealed trait SymChange:
    def apply(old: Symbolic): Symbolic
case object NoChange extends SymChange:
    def apply(old: Symbolic): Symbolic = old
case class SymReplace(from: Symbolic, to: Symbolic) extends SymChange:
    def apply(old: Symbolic): Symbolic =
        def visit(e: SchemeExp): SchemeExp =
          if e == from.expr then to.expr
          else
              (e match
                  case SchemeFuncall(f, args, idn) =>
                    SchemeFuncall(visit(f), args.map(visit), idn)
                  case SchemeVar(_) | SchemeVarLex(_, _) | SchemeValue(_, _) => e
                  case _                                                     => throw new Exception(s"unsupported symbolic expression $e")
          )

        Symbolic(visit(old.expr))

case class SymbolicStore(mapping: Map[String, Symbolic]):
    def roots: Set[String] =
      Symbolic.identifiers(mapping.values.map(_.expr)).toSet

    /**
     * Reindex the symbolic store based on the given base.
     *
     * For example:
     *
     * a -> x3; b -> x4
     *
     * Can be reindex with base 3 to
     *
     * a -> x0; b -> x1
     */
    def reindex(base: Int): SymbolicStore =
      SymbolicStore(mapping.mapValues(_.reindex(base)).toMap)

    /**
     * Add a binding to the symbolic store, and allocate a fresh variable if required @
     *
     * @param name
     *   the name of the identifier to add to the store
     * @param sym
     *   the symbolic rerpresentation of the identifiers
     * @param fresh
     *   a monadic operation that generates a fresh unique identifier
     * @param allowed
     *   a predicate that returns true if the expression is allowed to be retained
     *
     * @return
     *   a pair of an update store and optionally the original symbolic binding if it was replaced by a fresh identifier
     */
    def add[M[_]: Monad](
        name: String,
        sym: Symbolic,
        fresh: M[SchemeExp],
        allowed: Symbolic => Boolean = (_) => true
      ): M[(SymbolicStore, SymChange)] =
        import maf.core.Monad.MonadSyntaxOps
        val symbolic = if allowed(sym) then Monad[M].unit((sym, NoChange)) else fresh.map(id => (Symbolic(id), SymReplace(sym, Symbolic(id))))
        symbolic.map(sym => (this.copy(mapping = mapping + (name -> sym._1)), sym._2))

    /** Add a list of bindings to the symbolic store */
    def addAll[M[_]: Monad](
        bindings: List[(String, Symbolic)],
        fresh: M[SchemeExp],
        allowed: Symbolic => Boolean = (_) => true
      ): M[(SymbolicStore, List[SymChange])] =
        import maf.core.Monad.MonadIterableOps
        bindings.foldLeftM((this, List()))((result: (SymbolicStore, List[SymChange]), binding: (String, Symbolic)) =>
          binding match
              case (name, symbolic) =>
                for
                    afterUpdate <- result._1.add(name, symbolic, fresh, allowed)
                    (newStore, change) = afterUpdate
                yield (newStore, change :: result._2)
        )

    def gc(roots: List[String]): SymbolicStore =
        println(s"got roots $roots for $mapping")
        this.copy(mapping = mapping.filterKeys(roots.contains).toMap)

object SymbolicStore:
    /** A symbolic store where only variables are allowed */
    def onlyVarsAllowed(symbolic: Symbolic): Boolean =
      symbolic.expr match
          case SchemeVar(_) => true
          case _            => false

    def apply(): SymbolicStore = SymbolicStore(Map())

object PathStore:
    def onlyVars[M[_]: Monad](fresh: M[SchemeExp])(sym: Symbolic): M[Symbolic] = sym.expr match
        case SchemeVar(_) => Monad[M].unit(sym)
        case _            => fresh.map(Symbolic.apply(_))

/**
 * A path store associates an identifier with a symbolic expression that uses this identifier. It supports a form of garbage collection where only the
 * relevant constraints remain.
 *
 * Creating a finite set of path conditions:
 *   - Cutting of parts of the path condition that are not used any more
 *   - Making sure that symbolic representations cannot grow infinitely and that names of identifier are not infinite
 */
case class PathStore(pcs: Map[String, Set[Symbolic]] = Map(), cachedPc: Set[Symbolic] = Set()):
    import Symbolic.Implicits.*

    private def computeMapExtension(condition: Symbolic): Set[(String, Set[Symbolic])] =
      condition.identifiers.map(_ -> Set(condition))

    private def extendMap(condition: Symbolic, pcs: Map[String, Set[Symbolic]]): Map[String, Set[Symbolic]] =
      pcs
        .foldLeft(computeMapExtension(condition)) { case (acc, (k, v)) =>
          acc + (k -> v)
        }
        .toMap

    private def computeMap(path: Set[Symbolic]): Map[String, Set[Symbolic]] =
      path.foldLeft(Map[String, Set[Symbolic]]())((acc, cnd) => extendMap(cnd, acc))

    /** Extend the path condition with the given constraint */
    def extendPc(addition: Symbolic): PathStore =
        val newCachedPc = cachedPc + addition
        val newPcs = extendMap(addition, pcs)
        this.copy(pcs = newPcs, cachedPc = newCachedPc)

    /** Extend the path condition with the given list of symbolic constraints */
    def extendPc(additions: List[SchemeExp]): PathStore =
      additions.foldLeft(this)((pathStore, addition) => pathStore.extendPc(addition))

    /** Compute the linear version of the path condition (excluding duplicates) */
    def pc: List[SchemeExp] =
      cachedPc.map(_.expr).toList

    def vars: List[String] =
      pcs.keySet.toList

    /** Garbage collect the path condition, based on the variables that are required */
    def gc(roots: Set[String]): PathStore =
        val gced = pcs.filterKeys(k => roots.contains(k)).toMap
        val newCachedPc = gced.values.toSet.flatten
        this.copy(pcs = gced, cachedPc = newCachedPc)

    /** Find the lowest identifier (ie. x1 < x3) in the current path condition */
    private def findLowest: Int =
      cachedPc.flatMap(_.identifiers.filter(_.startsWith("x")).map(id => id.split('x')(1).toInt)) match
          case x if x.isEmpty => 0
          case x              => x.min

    /** Reindex a single symbolic expression */
    private def reindex(lowest: Int, sym: Symbolic): Symbolic =
      sym.reindex(lowest)

    private def reindex(lowest: Int, pc: Set[Symbolic]): Set[Symbolic] =
      pc.map(cnd => reindex(lowest, cnd))

    def lowest: Int = findLowest

    /**
     * Rename/reset the names of the identifiers based on the first available variable number.
     *
     * For example if we have in the path condition: (number? x3) /\ (x3 = 0) then this can be renamed to (number? x0) /\ (x0 = 0) without changing
     * its meaning.
     *
     * This is a heavy operation, to be used sparingly.
     */
    def reindex(lowest: Int): PathStore =
        val reindexed = reindex(lowest, cachedPc)
        val recomputedMap = computeMap(reindexed)
        this.copy(pcs = recomputedMap, cachedPc = reindexed)

    /**
     * Simplify the constraints in the path store, according to the given mapping function
     *
     * @param changeset
     *   a list of changes to apply
     */
    def clean(changeset: List[SymChange]): PathStore =
        val newCache =
          for constraint <- cachedPc
          yield changeset.foldLeft(constraint)((constraint, change) => change.apply(constraint))

        this.copy(pcs = computeMap(newCache), cachedPc = newCache)
