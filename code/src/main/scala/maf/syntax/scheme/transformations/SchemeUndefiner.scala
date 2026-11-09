package maf.syntax.scheme.transformations

import maf.syntax.*
import maf.syntax.scheme.*

import scala.collection.mutable

/**
 * Trait can be mixed in to obtain access to a "check" function, which checks whether a program is valid according to the specification with regards
 * to the context where internal definitions may occur
 */
trait UndefinerTester:
    sealed protected trait Result:
        def ||(other: => Result): Result = this match
            case NoError(_)      => other
            case Error(_)        => this
            case ErrorNoPosition => this

        /** Same as <code>||</code> but chains the return value of the previous one */
        def ||>(other: Boolean => Result): Result = this match
            case NoError(v) => other(v)
            case _          => this

        def isError: Boolean = this match
            case NoError(_) => false
            case _          => true

        def show: String = this match
            case NoError(_)      => "<no-error>"
            case Error(pos)      => s"<error at ${pos.show}>"
            case ErrorNoPosition => s"<error>"

    protected case class NoError(allowed: Boolean) extends Result
    protected case class Error(location: Option[Position]) extends Result
    protected case object ErrorNoPosition extends Result

    implicit def toResult(b: Boolean)(using allowed: Boolean): Result = if b then ErrorNoPosition else NoError(allowed)

    /**
     * Checks whether the given Scheme program contains a define in the wrong contexT.
     *
     * R5RS specifies that defiens may only occur on a top-level, or in the beginning of a body (so called internal defines).
     *
     * This function returns true if a define is in the wrong context.
     */
    protected def check(s: List[SchemeExp[Any]], allowed: Boolean): Result =
        s.foldLeft[Result](NoError(allowed))(_ || check(_, allowed))

    /**
     * Checks wether the given variable denotes an annotation.
     *
     * Annotations are currently used as sensitivity annoations, and start with "@"
     */
    private def isAnnotation(vrr: SchemeVarExp[Any]): Boolean =
        vrr.id.name.startsWith("@")

    /**
     * Checks the given list as a sequence of expressions.
     *
     * Will reject programs that have defines and expressions interleaved
     */
    private def checkSequence(s: List[SchemeExp[Any]])(allowed: Boolean): Result =
        given allowedB: Boolean = allowed
        s match
            case (SchemeDefineVariable(name, value, pos)) :: rest =>
                if allowed then check(value, false) || checkSequence(rest)(true) else Error(pos)
            case (e @ SchemeBegin(_, _)) :: rest =>
                check(e, allowed) ||> checkSequence(rest)
            case (vrr: SchemeVarExp[?]) :: rest if isAnnotation(vrr) =>
                // annotations of the form @... are ignored as expressions
                checkSequence(rest)(allowed)

            case x :: xs =>
                check(x, false) ||> checkSequence(xs)

            case Nil =>
                NoError(allowed)

    /** Checks whether the given expression contains defines in invalid contexts */
    protected def check(s: SchemeExp[Any], allowed: Boolean): Result =
        given allowedB: Boolean = allowed
        s match
            case SchemeLambda(name, args, body, ann, pos) =>
                checkSequence(body)(true)

            case SchemeVarArgLambda(name, args, vararg, body, ann, pos) =>
                checkSequence(body)(true)

            case SchemeFuncall(f, args, pos) =>
                check(f, false)

            case SchemeIf(cond, cons, alt, pos) =>
                check(cond, false) || check(cons, false) || check(alt, false)

            case SchemeLet(bindings, body, pos) =>
                bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body)(true)

            case SchemeLetStar(bindings, body, pos) =>
                bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body)(true)

            case SchemeLetrec(bindings, body, pos) =>
                bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body)(true)

            case SchemeSet(variable, value, pos) =>
                check(value, false)

            case SchemeSetLex(variable, lexAddr, value, pos) =>
                check(value, false)

            case SchemeBegin(exps, pos) =>
                checkSequence(exps)(allowed)

            case SchemeDefineVariable(name, value, pos) =>
                if allowed then check(value, false) else Error(pos)

            case SchemeVar(id)            => false
            case SchemeVarLex(id, lexAdr) => false
            case SchemeValue(value, pos)  => false
            case SchemeAssert(exp, _) =>
                check(exp, false)
end UndefinerTester

/**
 * Removes defines from a Scheme program, replacing them by letrec bindings. For example, `(define foo 1) (define (f x) x) (f foo)` is converted to
 * `(letrec ((foo 1) (f (lambda (x) x))) (f foo))`, which is semantically equivalent with respect to the end result.
 *
 * At the top level, all other expressions are bound to fresh variables (`_0`, `_1`, ...) as well, and the program evaluates to the last of these.
 * The result therefore has no definitions, but may refer to variables by name.
 *
 * The program is assumed to be compiled by the SchemeCompiler, and is checked not to contain defines in an invalid context.
 */
object SchemeUndefiner extends UndefinerTester:

    /** Undefines a program, which keeps the marker `Preluded` (as the definitions of the prelude become bindings of a letrec) */
    def undefine[F, M >: Preluded](exps: List[SchemeExp[F | Define]] & M): SchemeExp[F | Named] & M =
        keepPrelude(exps)(exps => SchemeBody(undefineExps(exps)))

    def undefineExps[F](exps: List[SchemeExp[F | Define]], topLevel: Boolean = true): List[SchemeExp[F | Named]] =
        val result = check(exps, true)
        if result.isError then
            throw new Exception(s"Malformed program, define in invalid context. First occurence of error at ${result.show} in $exps")
        new Undefiner().undefineBody(exps, topLevel)

    /** The bindings introduced by a body, in order. */
    private type Bindings[F] = mutable.ListBuffer[(Identifier, SchemeExp[F])]

    /** Undefines a single program, keeping a counter to generate fresh names for its top-level expressions. */
    private class Undefiner:
        private var counter = 0

        private def fresh(pos: Option[Position]): Identifier =
            val id = Identifier(s"_$counter", pos)
            counter += 1
            id

        /** Undefines a body, turning its defines (and, at the top level, all its expressions) into letrec bindings. */
        def undefineBody[F](body: List[SchemeExp[F | Define]], topLevel: Boolean): List[SchemeExp[F | Named]] =
            if topLevel then undefineTopLevel(body) else undefineNested(body)

        /** Undefines the top level of a program, which binds all of its expressions. */
        private def undefineTopLevel[F](body: List[SchemeExp[F | Define]]): List[SchemeExp[F | Named]] =
            val bds: Bindings[F] = mutable.ListBuffer.empty
            val exps = body.flatMap(undefine(_, bds, true))
            assert(exps.isEmpty) // all expressions at the top level are bound
            letrectifyTopLevel(bds.toList)

        /** Undefines a body that is nested in an expression. */
        private def undefineNested[F](body: List[SchemeExp[F | Define]]): List[SchemeExp[F]] =
            val bds: Bindings[F] = mutable.ListBuffer.empty
            val exps = body.flatMap(undefine(_, bds, false))
            letrectify(exps, bds.toList)

        /** Undefines an expression that is not part of a body, and therefore results in exactly one expression. */
        private def undefineSingle[F](exp: SchemeExp[F | Define], bds: Bindings[F]): SchemeExp[F] =
            val exps = undefine(exp, bds, false)
            assert(exps.size == 1)
            exps.head

        /**
         * Undefines an expression in a body with bindings `bds`. Defines are added to these bindings, and so are all other expressions at the top
         * level. Returns the expressions that remain in the body.
         */
        private def undefine[F](exp: SchemeExp[F | Define], bds: Bindings[F], topLevel: Boolean): List[SchemeExp[F]] =
            def emit(e: SchemeExp[F]): List[SchemeExp[F]] =
                if topLevel then
                    bds += (fresh(e.pos) -> e)
                    Nil
                else List(e)
            def single(e: SchemeExp[F | Define]): SchemeExp[F] = undefineSingle(e, bds)
            def bindings(bnd: List[(Identifier, SchemeExp[F | Define])]) = bnd.map((id, e) => (id, single(e)))
            def body(bdy: List[SchemeExp[F | Define]]): List[SchemeExp[F]] = undefineNested(bdy)
            exp match
                case SchemeLambda(name, args, bdy, ann, pos)               => emit(SchemeLambda(name, args, body(bdy), ann, pos))
                case SchemeVarArgLambda(name, args, vararg, bdy, ann, pos) => emit(SchemeVarArgLambda(name, args, vararg, body(bdy), ann, pos))
                case SchemeIf(cnd, csq, alt, pos)                          => emit(SchemeIf(single(cnd), single(csq), single(alt), pos))
                case SchemeFuncall(fun, args, pos)                         => emit(SchemeFuncall(single(fun), args.map(single), pos))
                case SchemeLet(bnd, bdy, pos)                              => emit(SchemeLet(bindings(bnd), body(bdy), pos))
                case SchemeLetStar(bnd, bdy, pos)                          => emit(SchemeLetStar(bindings(bnd), body(bdy), pos))
                case SchemeLetrec(bnd, bdy, pos)                           => emit(SchemeLetrec(bindings(bnd), body(bdy), pos))
                case SchemeSet(variable, value, pos)                       => emit(SchemeSet(variable, single(value), pos))
                case SchemeSetLex(variable, lexAddr, value, pos)           => emit(SchemeSetLex(variable, lexAddr, single(value), pos))
                case SchemeAssert(cnd, pos)                                => emit(SchemeAssert(single(cnd), pos))
                case e: SchemeVar                                          => emit(e)
                case e: SchemeVarLex                                       => emit(e)
                case e: SchemeValue                                        => emit(e)
                // begins do not introduce an additional scope
                case SchemeBegin(exps, pos) =>
                    val undefined = exps.flatMap(undefine(_, bds, topLevel))
                    if undefined.isEmpty then Nil else emit(SchemeBegin(undefined, pos))
                case SchemeDefineVariable(name, value, _) =>
                    bds += (name -> single(value))
                    Nil

    /** Binds the given bindings of a body around its remaining expressions. */
    private def letrectify[F](body: List[SchemeExp[F]], bindings: List[(Identifier, SchemeExp[F])]): List[SchemeExp[F]] =
        if bindings.isEmpty then body
        else List(SchemeLetrec(bindings, body, None))

    /** Binds the given bindings of the top level, in an expression that evaluates to (the variable bound to) the last of them. */
    private def letrectifyTopLevel[F](bindings: List[(Identifier, SchemeExp[F])]): List[SchemeExp[F | Named]] =
        if bindings.isEmpty then Nil
        else
            // This can be cleaner, but this avoids creating a top-level letrec when the entire expression is just a let. (todo: also on other levels and when no let is needed)
            bindings.last._2 match
                case lettishExp: SchemeLettishExp[?] if bindings.size == 1 => List(lettishExp)
                case _ => List(SchemeLetrec(bindings, List(SchemeVar(bindings.last._1)), None))
