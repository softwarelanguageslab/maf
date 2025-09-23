package maf.language.scheme

import maf.core._

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
            case (RacketRequire(_, _)) :: rest =>
                checkSequence(rest)(true)
            case (RacketProvide(_, _)) :: rest =>
                checkSequence(rest)(true)
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

            // CScheme
            case CSchemeFork(body, _) =>
                check(body, false) // TODO: check if this is correct. Defines allowed at the start of the body
            case CSchemeJoin(texp, _) =>
                check(texp, false)

            // Taints
            case SchemeSource(name, _)    => false
            case SchemeSink(name, _)      => false
            case SchemeSanitizer(name, _) => false
            
            // Racket modules
            case RacketRequire(clauses, _) =>
                clauses.foldLeft[Result](false)((result, clause) => result || check(clause, false))
            case RacketProvide(clauses, _) =>
                clauses.foldLeft[Result](false)((result, clause) => result || check(clause, false))
            case RacketModuleExpose(_, _)  => false
            case RacketModuleLoad(_, _, _) => false

            case mod @ RacketModule(_, _, _, _, _, bdy, _) =>
                check(bdy, true)

            case _ =>
                //false
                throw new Exception(s"unrecongized expression $s")
end UndefinerTester

class BaseSchemeUndefiner:
    def undefine(exps: List[SchemeExp]): SchemeExp =
        SchemeBody(SchemeMonadicUndefiner.undefineExps(exps))

object SchemeUndefiner extends BaseSchemeUndefiner
