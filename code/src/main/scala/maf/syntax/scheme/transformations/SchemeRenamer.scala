package maf.syntax.scheme.transformations

import maf.syntax.Identifier
import maf.syntax.scheme.*

/** Marks programs in which every variable has a unique name, which only renaming establishes */
opaque type UniqueNames = Any

private def unique[P](prg: P): P & UniqueNames = prg

/**
 * Object that provides a method to rename variables in a Scheme program in order to have only unique names. For example, (let ((x 1)) (let ((x 2))
 * x)) will be converted to (let ((_x_0 1)) (let ((_x_1 2)) _x_1)). This is useful to perform ANF conversion.
 *
 * Only programs without definitions and assignments can be renamed, so programs have to be undefined and boxed first:
 *   - the names of definitions are not renamed, and a definition that shadows a renamed variable is not taken into account;
 *   - boxing assignments introduces references to definitions of the prelude (such as `#%cons`) by their original names, which renaming
 *     changes.
 */
object SchemeRenamer:

    /** Maps each variables to their alpha-renamed version (eg. x -> _x_0) */
    type NameMap = Map[String, String]

    /** Map each variables to the number of times it is bound */
    type CountMap = Map[String, Int]

    /** Renames the variables of a program to unique names, which keeps the marker `Preluded` (as every variable keeps referring to the same binding) */
    def rename[M >: Preluded](exp: SchemeExp[Named] & M): SchemeExp[Named] & UniqueNames & M =
        unique(keepPrelude(exp)(exp => rename(exp, Map[String, String](), Map[String, Int]())._1))

    def rename[F <: Named](
        exp: SchemeExp[F],
        names: NameMap,
        count: CountMap
      ): (SchemeExp[F], CountMap) = exp match
        case SchemeLambda(name, args, body, ann, pos) =>
            countl(args, names, count) match
                case (args1, names1, count1) =>
                    renameList(body, names1, count1) match
                        case (body1, count2) => (SchemeLambda(name.map(n => names.getOrElse(n, n)), args1, body1, ann, pos), count2)
        case SchemeVarArgLambda(name, args, vararg, body, ann, pos) =>
            countl(args ++ List(vararg), names, count) match
                case (args1, names1, count1) =>
                     renameList(body, names1, count1) match
                         case (body1, count2) => (SchemeVarArgLambda(name.map(n => names.getOrElse(n ,n)), args1.init, args1.last, body1, ann, pos), count2)
        case SchemeFuncall(f, args, pos) =>
            rename(f, names, count) match
                case (f1, count1) =>
                    renameList(args, names, count1) match
                        case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
        case SchemeIf(cond, cons, alt, pos) =>
            rename(cond, names, count) match
                case (cond1, count1) =>
                    rename(cons, names, count1) match
                        case (cons1, count2) =>
                            rename(alt, names, count2) match
                                case (alt1, count3) => (SchemeIf(cond1, cons1, alt1, pos), count3)
        case SchemeLet(bindings, body, pos) =>
            countl(bindings.map(_._1), names, count) match
                /* Use old names for expressions of bindings */
                case (variables, names1, count1) =>
                    renameList(bindings.map(_._2), names, count1) match
                        case (exps, count2) =>
                            renameList(body, names1, count2) match
                                case (body1, count3) => (SchemeLet(variables.zip(exps), body1, pos), count3)
        case SchemeLetStar(bindings, body, pos) =>
            renameLetStarBindings(bindings, names, count) match
                case (bindings1, names1, count1) =>
                    renameList(body, names1, count1) match
                        case (body1, count2) => (SchemeLetStar(bindings1, body1, pos), count2)
        case SchemeLetrec(bindings, body, pos) =>
            countl(bindings.map(_._1), names, count) match
                /* Use new names for expressions of bindings */
                case (variables, names1, count1) =>
                    renameList(bindings.map(_._2), names1, count1) match
                        case (exps, count2) =>
                            renameList(body, names1, count2) match
                                case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1, pos), count3)
        case SchemeBegin(body, pos) =>
            renameList(body, names, count) match
                case (body1, count1) => (SchemeBegin(body1, pos), count1)
        case SchemeAssert(exp, pos) =>
            rename(exp, names, count) match
                case (exp1, count1) => (SchemeAssert(exp1, pos), count1)
        case SchemeVar(id) =>
            names.get(id.name) match
                case Some(n) => (SchemeVar(Identifier(n, id.pos)), count)
                case None    => (SchemeVar(Identifier(id.name, id.pos)), count) /* keep original name */
        case SchemeValue(v, pos) =>
            (SchemeValue(v, pos), count)

    /** Renames a list of expressions executed sequentially (eg. within a begin) */
    def renameList[F <: Named](
        exps: List[SchemeExp[F]],
        names: NameMap,
        count: CountMap
      ): (List[SchemeExp[F]], CountMap) = exps match
        case exp :: rest =>
            val (exp1, count1) = rename(exp, names, count)
            val (rest1, count2) = renameList(rest, names, count1)
            (exp1 :: rest1, count2)
        case Nil => (Nil, count)

    def renameLetStarBindings[F <: Named](
        bindings: List[(Identifier, SchemeExp[F])],
        names: NameMap,
        count: CountMap
      ): (List[(Identifier, SchemeExp[F])], NameMap, CountMap) =
        bindings match
            case (v, e) :: rest =>
                count1(v, names, count) match
                    /* use old names, as with a let* the variable is not yet bound in its
                     * definition */
                    case (v1, names1, count1) =>
                        rename(e, names, count1) match
                            case (e1, count2) =>
                                renameLetStarBindings(rest, names1, count2) match
                                    case (rest1, names2, count3) =>
                                        ((v1, e1) :: rest1, names2, count3)
            case Nil => (Nil, names, count)

    /** To be called when a new variable is introduced in the scope. Adds it to the name map and count map */
    def count1(
        variable: Identifier,
        names: NameMap,
        count: CountMap
      ): (Identifier, NameMap, CountMap) =
        val c: Int = count.get(variable.name) match
            case Some(x) => x + 1
            case None    => 0
        // the counter is separated from the name, as the name can end in digits (otherwise, `a` with counter 10 and `a1` with counter 0 would
        // both become `_a10`)
        val n = s"_${variable.name}_$c"
        (Identifier(n, variable.pos), names + (variable.name -> n), count + (variable.name -> c))

    /** Same as count1 but for a list of variables */
    def countl(
        variables: List[Identifier],
        names: NameMap,
        count: CountMap
      ): (List[Identifier], NameMap, CountMap) =
        variables.foldLeft((List[Identifier](), names, count))((st: (List[Identifier], NameMap, CountMap), v: Identifier) =>
            st match {
                case (l, ns, cs) =>
                    count1(v, ns, cs) match {
                        case (v1, ns1, cs1) => (v1 :: l, ns1, cs1)
                    }
            }
        ) match
            case (l, ns, cs) => (l.reverse, ns, cs)
