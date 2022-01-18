package maf.cli.experiments.preprocessing

import maf.language.scheme.*
import maf.bench.scheme.*
import maf.util.Reader
import maf.core.Identity
import scala.util.Try

object DefineTester:
    sealed trait Result:
        def ||(other: => Result): Result = this match
            case NoError         => other
            case Error(_)        => this
            case ErrorNoIdentity => this
        def isError: Boolean = this match
            case NoError => false
            case _       => true

        def show: String = this match
            case NoError         => "<no-error>"
            case Error(idn)      => s"<error at $idn>"
            case ErrorNoIdentity => s"<error>"

    case object NoError extends Result
    case class Error(location: Identity) extends Result
    case object ErrorNoIdentity extends Result

    implicit def toResult(b: Boolean): Result = if b then ErrorNoIdentity else NoError

    /**
     * Checks whether the given Scheme program contains a define in the wrong contexT.
     *
     * R5RS specifies that defiens may only occur on a top-level, or in the beginning of a body (so called internal defines).
     *
     * This function returns true if a define is in the wrong context.
     */
    private def check(s: List[SchemeExp], allowed: Boolean): Result =
      s.foldLeft[Result](NoError)(_ || check(_, allowed))

    private def checkSequence(s: List[SchemeExp], allowed: Boolean): Result =
      s match
          case (SchemeDefineVariable(name, value, idn)) :: rest =>
            if allowed then check(value, false) || check(rest, true) else Error(idn)
          case x :: xs =>
            check(x, false) || checkSequence(xs, false)

          case Nil => false

    private def check(s: SchemeExp, allowed: Boolean): Result = s match
        case SchemeLambda(name, args, body, idn) =>
          checkSequence(body, true)

        case SchemeVarArgLambda(name, args, vararg, body, idn) =>
          checkSequence(body, true)

        case SchemeFuncall(f, args, idn) =>
          check(f, false)

        case SchemeIf(cond, cons, alt, idn) =>
          check(cond, false) || check(cons, false) || check(alt, false)

        case SchemeLet(bindings, body, idn) =>
          bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body, true)

        case SchemeLetStar(bindings, body, idn) =>
          bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body, true)

        case SchemeLetrec(bindings, body, idn) =>
          bindings.map(_._2).foldLeft[Result](false)(_ || check(_, false)) || checkSequence(body, true)

        case SchemeSet(variable, value, idn) =>
          check(value, false)

        case SchemeSetLex(variable, lexAddr, value, idn) =>
          check(value, false)

        case SchemeBegin(exps, idn) =>
          checkSequence(exps, allowed)

        case SchemeDefineVariable(name, value, idn) =>
          if allowed then check(value, false) else Error(idn)

        case SchemeVar(id)            => false
        case SchemeVarLex(id, lexAdr) => false
        case SchemeValue(value, idn)  => false
        case SchemeAssert(exp, _) =>
          check(exp, false)
        case _ => throw new Exception(s"unrecongized expression $s")

    def main(args: Array[String]): Unit =
      if args.size != 1 then println("Usage: DefineTester benchmarks")
      else
          val directory = args(0)
          val programs = SchemeBenchmarkPrograms.fromFolderR(directory)(".DS_Store")
          val parsed = programs.map(name => (Reader.loadFile(name), name)).flatMap { (s, name) =>
            Try(SchemeParser.parse(s)).toOption.map((e) => (e, name))
          }
          val results = parsed.map { case (e, name) =>
            (check(e, true), name)
          }

          for (result, program) <- results do
              if result.isError then println(s"Program $program contains defines on invalid locations, ${result.show}")

end DefineTester
