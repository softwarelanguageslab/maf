package maf.modular.scv

import maf.language.scheme._
import maf.core.Identifier

/** A symbolic Scheme expression together with all of its identifiers */
case class Symbolic(expr: SchemeExp, identifiers: Set[String])

object Symbolic:
    /** Alternative constructor for `Symbolic`, computes the set of identifiers using the free variables from the expression */
    def apply(expr: SchemeExp): Symbolic =
      Symbolic(expr, expr.fv)

/**
 * A path store associates an identifier with a symbolic expression that uses this identifier. It supports a form of garbage collection where only the
 * relevant constraints remain
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
