package maf.modular.scv

import maf.language.scheme._
import maf.core.Identifier

/** A symbolic Scheme expression together with all of its identifiers */
case class Symbolic(expr: SchemeExp, identifiers: Set[String])

object Symbolic:
    /** Alternative constructor for `Symbolic`, computes the set of identifiers using the free variables from the expression */
    def apply(expr: SchemeExp): Symbolic =
      Symbolic(expr, expr.fv)

    /** Implicitly converts any SchemeExp to an instance of `Symbolic` */
    implicit def toSymbolic(exp: SchemeExp): Symbolic =
      Symbolic(exp)

    def reindex(lowest: Int, vr: String): String =
      s"x${vr.split('x')(0).toInt - lowest}"

/**
 * A path store associates an identifier with a symbolic expression that uses this identifier. It supports a form of garbage collection where only the
 * relevant constraints remain.
 *
 * Creating a finite set of path conditions:
 *   - Cutting of parts of the path condition that are not used any more
 *   - Making sure that symbolic representations cannot grow infinitely and that names of identifier are not infinite
 */
case class PathStore(pcs: Map[String, List[Symbolic]] = Map(), cachedPc: Option[List[Symbolic]] = None):

    private def computeMapExtension(condition: Symbolic): Set[(String, List[Symbolic])] =
      condition.identifiers.map(_ -> List(condition))

    private def extendMap(condition: Symbolic, pcs: Map[String, List[Symbolic]]): Map[String, List[Symbolic]] =
      pcs
        .foldLeft(computeMapExtension(condition)) { case (acc, (k, v)) =>
          acc + (k -> v)
        }
        .toMap

    private def computeMap(path: List[Symbolic]): Map[String, List[Symbolic]] =
      path.foldLeft(Map[String, List[Symbolic]]())((acc, cnd) => extendMap(cnd, acc))

    /** Extend the path condition with the given constraint */
    def extendPc(addition: Symbolic): PathStore =
        val newCachedPc = Some(addition :: cachedPc.getOrElse(List()))
        val newPcs = extendMap(addition, pcs)
        this.copy(pcs = newPcs, cachedPc = newCachedPc)

    /** Compute the linear version of the path condition (excluding duplicates) */
    def pc: List[SchemeExp] =
      cachedPc.getOrElse(List()).map(_.expr)

    /** Garbage collect the path condition, based on the variables that are required */
    def gc(roots: Set[String]): PathStore =
        val gced = pcs.filterKeys(k => roots.contains(k)).toMap
        val newCachedPc = gced.values.toSet.flatten.toList
        this.copy(pcs = gced, cachedPc = Some(newCachedPc))

    /** Find the lowest identifier (ie. x1 < x3) in the current path condition */
    private def findLowest: Int =
      cachedPc.get.flatMap(_.identifiers.map(id => id.split('x')(1).toInt)).min

    /** Reindex a single symbolic expression */
    private def reindex(lowest: Int, sym: Symbolic): Symbolic =
        def visit(exp: SchemeExp): SchemeExp = exp match
            case SchemeFuncall(f, args, idn) =>
              SchemeFuncall(visit(f), args.map(visit), idn)
            case SchemeVar(id) =>
              SchemeVar(Identifier(Symbolic.reindex(lowest, id.name), id.idn))

            case SchemeVarLex(id, lexAdr) =>
              SchemeVarLex(Identifier(Symbolic.reindex(lowest, id.name), id.idn), lexAdr)
            case v @ SchemeValue(value, idn) => v
            case _                           => throw new Exception(s"unsupported symbolic expression $exp")

        Symbolic(visit(sym.expr))

    private def reindex(lowest: Int, pc: List[Symbolic]): List[Symbolic] =
      pc.map(cnd => reindex(lowest, cnd))

    /**
     * Rename/reset the names of the identifiers based on the first available variable number.
     *
     * For example if we have in the path condition: (number? x3) /\ (x3 = 0) then this can be renamed to (number? x0) /\ (x0 = 0) without changing
     * its meaning.
     *
     * This is a heavy operation, to be used sparingly.
     */
    def reindex: PathStore =
        val lowest = findLowest
        val reindexed = reindex(lowest, cachedPc.get)
        val recomputedMap = computeMap(reindexed)
        this.copy(pcs = recomputedMap, cachedPc = Some(reindexed))
