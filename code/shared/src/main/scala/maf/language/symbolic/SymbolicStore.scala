package maf.language.symbolic

import maf.core.Address

/** Utility functions to talk about mappings from addresses to symbolic values */
object SymbolicStore:
    import Symbolic.*

    /** Returns a list of symbolic variables */
    def variables(s: Symbolic): List[String] = s match
        case Funcall(fexp, fargs, _) =>
            variables(fexp) ++ fargs.flatMap(variables)
        case VarId(s) if s.startsWith("x") => List(s)
        case _                             => List()

    def variables(s: Map[Address, Symbolic]): List[String] = s.values.flatMap(variables).toList
    //def variableExpressions(s: Map[Address, Symbolic]): List[Var] = s.values.flatMap(Symbolic.variableExpressions).toList

    def lowest(variables: List[String]): Int =
        lowestOpt(variables).getOrElse(0)

    def lowestOpt(variables: List[String]): Option[Int] =
        if variables.size == 0 then None else Some(variables.map(_.split('x')(1).toInt).min)

    def lowestOpt(vars: Map[Address, Symbolic]): Option[Int] =
        lowestOpt(variables(vars))

    def highest(variables: List[String]): Int =
        if variables.size == 0 then 0 else variables.map(_.split('x')(1).toInt).max

    def highest(sstore: Map[Address, Symbolic]): Int =
        highest(sstore.values.flatMap(variables).toList)

    /** Based on the path condition, reindex the variables in the symbolic store. */
    def reindex(l: Int, sstore: Map[Address, Symbolic]): Map[Address, Symbolic] =
        val actualChanges = if l != 0 then variables(sstore).map(v => SymReplace(VarId(v), VarId(s"x${v.split('x')(1).toInt - l}"))) else List()
        sstore.mapValues(vlu => actualChanges.foldLeft(vlu)((vlu, change) => change.apply(Assertion(vlu)).asInstanceOf[Assertion].contents)).toMap
