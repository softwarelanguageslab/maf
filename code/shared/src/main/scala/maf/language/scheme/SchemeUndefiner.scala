package maf.language.scheme

import maf.core._

/**
 * Remove defines from a Scheme expression, replacing them by let bindings. For example: (define foo 1) (define (f x) x) (f foo) Will be converted to:
 * (letrec ((foo 1) (f (lambda (x) x))) (f foo)) Which is semantically equivalent with respect to the end result
 */
trait BaseSchemeUndefiner extends UndefinerTester:
    import scala.util.control.TailCalls._

    /** Override to disable checking whether defines only occur in valid contexts */
    protected val checkProgram = true

    def undefine(exps: List[SchemeExp]): SchemeExp =
        val result = check(exps, true)
        if checkProgram && result.isError then
            throw new Exception(s"Malformed program, define in invalid context. First occurrance of error at ${result.show} in $exps")
        else undefine(exps, List(), None).result

    def undefine(
        exps: List[SchemeExp],
        defs: List[(Identifier, SchemeExp)],
        idn: Option[Identity]
      ): TailRec[SchemeExp] =
      exps match
          case Nil => done(SchemeBegin(Nil, Identity.none))
          case SchemeDefineVariable(name, value, pos) :: rest =>
            tailcall(undefine1(value)).flatMap(v => tailcall(undefine(rest, (name, v) :: defs, idn.orElse(Some(pos)))))
          case _ :: _ =>
            tailcall(undefineBody(exps)).map { bdy =>
              if defs.isEmpty then SchemeBody(bdy)
              else SchemeLetrec(defs.reverse, if bdy.nonEmpty then bdy else List(SchemeBody(bdy)), idn.get)
            }

    def trampolineM[A, B](f: A => TailRec[B], l: List[A]): TailRec[List[B]] = l match
        case Nil => done(Nil)
        case x :: xs =>
          tailcall(f(x)).flatMap(y => tailcall(trampolineM(f, xs)).flatMap(ys => done(y :: ys)))

    def undefine1(exp: SchemeExp): TailRec[SchemeExp] = undefine(List(exp), List(), None)

    def undefineExp(exp: SchemeExp): TailRec[SchemeExp] = exp match
        case SchemeLambda(name, args, body, pos) =>
          tailcall(undefineBody(body)).map(b => SchemeLambda(name, args, b, pos))
        case SchemeVarArgLambda(name, args, vararg, body, pos) =>
          tailcall(undefineBody(body)).map(b => SchemeVarArgLambda(name, args, vararg, b, pos))
        case SchemeFuncall(f, args, pos) =>
          tailcall(undefine1(f)).flatMap(fun => trampolineM(undefine1, args).map(argsv => SchemeFuncall(fun, argsv, pos)))
        case SchemeIf(cond, cons, alt, pos) =>
          tailcall(undefine1(cond)).flatMap(condv =>
            tailcall(undefine1(cons)).flatMap(consv => tailcall(undefine1(alt)).map(altv => SchemeIf(condv, consv, altv, pos)))
          )
        case SchemeLet(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLet(bindingsv, bodyv, pos)))
        case SchemeLetStar(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLetStar(bindingsv, bodyv, pos)))
        case SchemeLetrec(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLetrec(bindingsv, bodyv, pos)))
        case SchemeSet(variable, value, pos) =>
          tailcall(undefine1(value)).map(v => SchemeSet(variable, v, pos))
        case SchemeBegin(exps, pos) =>
          tailcall(undefineBody(exps)).map(expsv => SchemeBegin(expsv, pos))
        case SchemeAssert(exp, pos) =>
          for expUndef <- tailcall(undefine1(exp))
          yield SchemeAssert(expUndef, pos)
        case SchemeVar(id)           => done(SchemeVar(id))
        case SchemeValue(value, pos) => done(SchemeValue(value, pos))
        case _                       => throw new Exception(s"Unsupported scheme expression $exp")

    def undefineBody(exps: List[SchemeExp]): TailRec[List[SchemeExp]] = exps match
        case Nil                                => done(Nil)
        case SchemeDefineVariable(_, _, _) :: _ => tailcall(undefine(exps, List(), None)).map(v => List(v))
        case exp :: rest                        => undefineExp(exp).flatMap(e1 => tailcall(undefineBody(rest)).flatMap(e2 => done(e1 :: e2)))

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

    private def checkSequence(s: List[SchemeExp])(allowed: Boolean): Result =
        given allowedB: Boolean = allowed
        s match
            case (SchemeDefineVariable(name, value, idn)) :: rest =>
              if allowed then check(value, false) || checkSequence(rest)(true) else Error(idn)
            case (e @ SchemeBegin(_, _)) :: rest =>
              check(e, allowed) ||> checkSequence(rest)
            case x :: xs =>
              check(x, false) ||> checkSequence(xs)

            case Nil =>
              NoError(allowed)

    protected def check(s: SchemeExp, allowed: Boolean): Result =
        given allowedB: Boolean = allowed
        s match
            case SchemeLambda(name, args, body, idn) =>
              checkSequence(body)(true)

            case SchemeVarArgLambda(name, args, vararg, body, idn) =>
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
              check(body, true) // TODO: check if this is correct. Defines allowed at the start of the body
            case CSchemeJoin(texp, _) =>
              check(texp, false)

            // Change expressions
            case SchemeCodeChange(old, nw, _) =>
              check(old, allowed) || check(nw, allowed)

            case _ =>
              false
//throw new Exception(s"unrecongized expression $s")
end UndefinerTester

object SchemeUndefiner extends BaseSchemeUndefiner
