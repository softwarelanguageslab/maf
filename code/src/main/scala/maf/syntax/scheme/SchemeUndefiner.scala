package maf.syntax.scheme

import maf.syntax.*

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
            case ErrorNoIdentity => this

        /** Same as <code>||</code> but chains the return value of the previous one */
        def ||>(other: Boolean => Result): Result = this match
            case NoError(v) => other(v)
            case _          => this

        def isError: Boolean = this match
            case NoError(_) => false
            case _          => true

        def show: String = this match
            case NoError(_)      => "<no-error>"
            case Error(idn)      => s"<error at $idn>"
            case ErrorNoIdentity => s"<error>"

    protected case class NoError(allowed: Boolean) extends Result
    protected case class Error(location: Identity) extends Result
    protected case object ErrorNoIdentity extends Result

    implicit def toResult(b: Boolean)(using allowed: Boolean): Result = if b then ErrorNoIdentity else NoError(allowed)

    /**
     * Checks whether the given Scheme program contains a define in the wrong contexT.
     *
     * R5RS specifies that defiens may only occur on a top-level, or in the beginning of a body (so called internal defines).
     *
     * This function returns true if a define is in the wrong context.
     */
    protected def check(s: List[SchemeExp], allowed: Boolean): Result =
        s.foldLeft[Result](NoError(allowed))(_ || check(_, allowed))

    /**
     * Checks wether the given variable denotes an annotation.
     *
     * Annotations are currently used as sensitivity annoations, and start with "@"
     */
    private def isAnnotation(vrr: SchemeVarExp): Boolean =
        vrr.id.name.startsWith("@")

    /**
     * Checks the given list as a sequence of expressions.
     *
     * Will reject programs that have defines and expressions interleaved
     */
    private def checkSequence(s: List[SchemeExp])(allowed: Boolean): Result =
        given allowedB: Boolean = allowed
        s match
            case (SchemeDefineVariable(name, value, idn)) :: rest =>
                if allowed then check(value, false) || checkSequence(rest)(true) else Error(idn)
            case (e @ SchemeBegin(_, _)) :: rest =>
                check(e, allowed) ||> checkSequence(rest)
            case (vrr: SchemeVarExp) :: rest if isAnnotation(vrr) =>
                // annotations of the form @... are ignored as expressions
                checkSequence(rest)(allowed)

            case x :: xs =>
                check(x, false) ||> checkSequence(xs)

            case Nil =>
                NoError(allowed)

    /** Checks whether the given expression contains defines in invalid contexts */
    protected def check(s: SchemeExp, allowed: Boolean): Result =
        given allowedB: Boolean = allowed
        s match
            case SchemeLambda(name, args, body, ann, idn) =>
                checkSequence(body)(true)

            case SchemeVarArgLambda(name, args, vararg, body, ann, idn) =>
                checkSequence(body)(true)

            case SchemeFuncall(f, args, idn) =>
                check(f, false)

            case SchemeIf(cond, cons, alt, idn) =>
                check(cond, false) || check(cons, false) || check(alt, false)

            case SchemeLet(bindings, body, idn) =>
                bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body)(true)

            case SchemeLetStar(bindings, body, idn) =>
                bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body)(true)

            case SchemeLetrec(bindings, body, idn) =>
                bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body)(true)

            case SchemeSet(variable, value, idn) =>
                check(value, false)

            case SchemeSetLex(variable, lexAddr, value, idn) =>
                check(value, false)

            case SchemeBegin(exps, idn) =>
                checkSequence(exps)(allowed)

            case SchemeDefineVariable(name, value, idn) =>
                if allowed then check(value, false) else Error(idn)

            case SchemeVar(id)            => false
            case SchemeVarLex(id, lexAdr) => false
            case SchemeValue(value, idn)  => false
            case SchemeAssert(exp, _) =>
                check(exp, false)
end UndefinerTester

/**
 * Removes defines from a Scheme program, replacing them by letrec bindings. For example, `(define foo 1) (define (f x) x) (f foo)` is converted to
 * `(letrec ((foo 1) (f (lambda (x) x))) (f foo))`, which is semantically equivalent with respect to the end result.
 *
 * At the top level, all other expressions are bound to fresh variables (`_0`, `_1`, ...) as well, and the program evaluates to the last of these.
 *
 * The program is assumed to be compiled by the SchemeCompiler, and is checked not to contain defines in an invalid context.
 */
object SchemeUndefiner extends UndefinerTester:

    def undefine(exps: List[SchemeExp]): SchemeExp =
        SchemeBody(undefineExps(exps))

    def undefineExps(exps: List[SchemeExp], topLevel: Boolean = true): List[SchemeExp] =
        val result = check(exps, true)
        if result.isError then
            throw new Exception(s"Malformed program, define in invalid context. First occurence of error at ${result.show} in $exps")
        new Undefiner().undefineBody(exps, topLevel)

    /** The bindings introduced by a body, in order. */
    private type Bindings = mutable.ListBuffer[(Identifier, SchemeExp)]

    /** Undefines a single program, keeping a counter to generate fresh names for its top-level expressions. */
    private class Undefiner:
        private var counter = 0

        private def fresh(idn: Identity): Identifier =
            val id = Identifier(s"_$counter", idn)
            counter += 1
            id

        /** Undefines a body, turning its defines (and, at the top level, all its expressions) into letrec bindings. */
        def undefineBody(body: List[SchemeExp], topLevel: Boolean): List[SchemeExp] =
            val bds: Bindings = mutable.ListBuffer.empty
            val exps = body.flatMap(undefine(_, bds, topLevel))
            letrectify(exps, bds.toList, topLevel)

        /** Undefines an expression that is not part of a body, and therefore results in exactly one expression. */
        private def undefineSingle(exp: SchemeExp, bds: Bindings): SchemeExp =
            val exps = undefine(exp, bds, false)
            assert(exps.size == 1)
            exps.head

        /**
         * Undefines an expression in a body with bindings `bds`. Defines are added to these bindings, and so are all other expressions at the top
         * level. Returns the expressions that remain in the body.
         */
        private def undefine(exp: SchemeExp, bds: Bindings, topLevel: Boolean): List[SchemeExp] =
            def emit(e: SchemeExp): List[SchemeExp] =
                if topLevel then
                    bds += (fresh(e.idn) -> e)
                    Nil
                else List(e)
            def single(e: SchemeExp): SchemeExp = undefineSingle(e, bds)
            def bindings(bnd: List[(Identifier, SchemeExp)]) = bnd.map((id, e) => (id, single(e)))
            def body(bdy: List[SchemeExp]) = undefineBody(bdy, false)
            exp match
                case SchemeLambda(name, args, bdy, ann, idn)               => emit(SchemeLambda(name, args, body(bdy), ann, idn))
                case SchemeVarArgLambda(name, args, vararg, bdy, ann, idn) => emit(SchemeVarArgLambda(name, args, vararg, body(bdy), ann, idn))
                case SchemeIf(cnd, csq, alt, idn)                          => emit(SchemeIf(single(cnd), single(csq), single(alt), idn))
                case SchemeFuncall(fun, args, idn)                         => emit(SchemeFuncall(single(fun), args.map(single), idn))
                case SchemeLet(bnd, bdy, idn)                              => emit(SchemeLet(bindings(bnd), body(bdy), idn))
                case SchemeLetStar(bnd, bdy, idn)                          => emit(SchemeLetStar(bindings(bnd), body(bdy), idn))
                case SchemeLetrec(bnd, bdy, idn)                           => emit(SchemeLetrec(bindings(bnd), body(bdy), idn))
                case SchemeSet(variable, value, idn)                       => emit(SchemeSet(variable, single(value), idn))
                case SchemeSetLex(variable, lexAddr, value, idn)           => emit(SchemeSetLex(variable, lexAddr, single(value), idn))
                case SchemeAssert(cnd, idn)                                => emit(SchemeAssert(single(cnd), idn))
                case _: SchemeVar | _: SchemeVarLex | _: SchemeValue       => emit(exp)
                // begins do not introduce an additional scope
                case SchemeBegin(exps, idn) =>
                    val undefined = exps.flatMap(undefine(_, bds, topLevel))
                    if undefined.isEmpty then Nil else emit(SchemeBegin(undefined, idn))
                case SchemeDefineVariable(name, value, _) =>
                    bds += (name -> single(value))
                    Nil

    private def letrectify(body: List[SchemeExp], bindings: List[(Identifier, SchemeExp)], topLevel: Boolean): List[SchemeExp] =
        if bindings.isEmpty then body
        else if topLevel then
            // This can be cleaner, but this avoids creating a top-level letrec when the entire expression is just a let. (todo: also on other levels and when no let is needed)
            bindings.last._2 match
                case lettishExp: SchemeLettishExp if bindings.size == 1 => List(lettishExp)
                case _ => List(SchemeLetrec(bindings, List(SchemeVar(bindings.last._1)), Identity.none))
        else List(SchemeLetrec(bindings, body, Identity.none))
