package maf.language.scheme.interpreter

import maf.core._
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.util.benchmarks.Timeout

import scala.concurrent.TimeoutException

class CPSSchemeInterpreter(
    cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (), // TODO: incoropate
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[ConcreteValues.Value]
       with ConcreteSchemePrimitives {

  // TODO: change for multi-threaded programs.
  override def run(
      program: SchemeExp,
      timeout: Timeout.T,
      version: Version
    ): ConcreteValues.Value = {
    setStore(initialSto)
    var state: State = Step(program, initialEnv, EndC())
    try {
      while (!timeout.reached)
        state = state match {
          case Step(exp, env, cc) => eval(exp, env, version, cc)
          case Kont(v, EndC(_))   => return v
          case Kont(v, cc)        => apply(v, cc)
        }
      throw new TimeoutException()
    } catch {
      // Use the continuations to print the stack trace.
      case StackedException(message) =>
        var cc = state.cc
        var msg: String = s"$message\n Callstack:"
        if (stack) {
          while ({
            msg = msg + s"\n * ${cc.toString}"
            cc = cc.cc
            cc != EndC() && cc != TrdC()
          }) ()
          msg += "\n **********"
        }
        throw new Exception(msg)
    }
  }

  case class StackedException(msg: String) extends Exception

  def stackedException[R](msg: String): R = throw StackedException(msg)

  /* ************************* */
  /* ***** CONTINUATIONS ***** */
  /* ************************* */

  sealed trait Continuation {
    val cc: Continuation // Needed to support the `stackedException` functionality.
  }

  case class AndC(
      exps: List[SchemeExp],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class BegC(
      exps: List[SchemeExp],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class AsmC(v: Identifier, cc: Continuation) extends Continuation

  case class AssC(
      v: Identifier,
      bnd: List[(Identifier, SchemeExp)],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class EndC(cc: Continuation = EndC()) extends Continuation // End of the program.
  case class RefC(cc: Continuation) extends Continuation

  case class TrdC(cc: Continuation = TrdC()) extends Continuation // End of a thread.

  /* ****************** */
  /* ***** STATES ***** */
  /* ****************** */

  sealed trait State {
    val cc: Continuation
  }

  case class Step(
      exp: SchemeExp,
      env: Env,
      cc: Continuation)
      extends State

  case class Kont(v: ConcreteValues.Value, cc: Continuation) extends State

  /* ********************** */
  /* ***** EVAL/APPLY ***** */
  /* ********************** */

  def eval(
      exp: SchemeExp,
      env: Env,
      version: Version,
      cc: Continuation
    ): State = exp match {
    case SchemeAnd(Nil, _)                             => Kont(Value.Bool(true), cc)
    case SchemeAnd(first :: Nil, _)                    => Step(first, env, cc)
    case SchemeAnd(first :: rest, _)                   => Step(first, env, AndC(rest, env, cc))
    case SchemeAssert(_, _)                            => Kont(Value.Void, cc) // Currently ignored.
    case SchemeBegin(Nil, _)                           => Kont(Value.Void, cc) // Allow empty begin (same than other interpreter).
    case SchemeBegin(first :: Nil, _)                  => Step(first, env, cc)
    case SchemeBegin(first :: rest, _)                 => Step(first, env, BegC(rest, env, cc))
    case SchemeCodeChange(old, _, _) if version == New => Step(old, env, cc)
    case SchemeCodeChange(_, nw, _) if version == Old  => Step(nw, env, cc)
    case SchemeDefineFunction(_, _, _, _)              => stackedException("Undefined expression expected.")
    case SchemeDefineVarArgFunction(_, _, _, _, _)     => stackedException("Undefined expression expected.")
    case SchemeDefineVariable(_, _, _)                 => stackedException("Undefined expression expected.")
    case SchemeFuncall(f, args, _)                     => ???
    case SchemeIf(cond, cons, alt, _)                  => ???
    case SchemeLambda(args, body, _)                   => ???
    case SchemeLet(bindings, body, _)                  => ???
    case SchemeLetStar(bindings, body, _)              => ???
    case SchemeLetrec(bindings, body, _)               => ???
    case SchemeNamedLet(name, bindings, body, _)       => ???
    case SchemeOr(exps, _)                             => ???
    case SchemePair(car, cdr, _)                       => ???
    case SchemeSet(variable, value, _)                 => ???
    case SchemeSetLex(variable, lexAddr, value, _)     => stackedException("Unsupported: lexical addresses.")
    case SchemeSplicedPair(splice, cdr, _)             => stackedException("NYI -- Unquote splicing")
    case SchemeValue(value, _)                         => Kont(evalLiteral(value, exp), cc)
    case SchemeVar(id) =>
      env.get(id.name).flatMap(lookupStoreOption).map(Kont(_, cc)).getOrElse(stackedException(s"Unbound variable $id at position ${id.idn}."))
    case SchemeVarArgLambda(args, vararg, body, _) => ???
    case SchemeVarLex(id, lexAddr)                 => stackedException("Unsupported: lexical addresses.")

    case CSchemeFork(body, _) => ???
    case CSchemeJoin(tExp, _) => ???

    case _ => throw new Exception(s"Unsupported expression type: ${exp.label}.")
  }

  def apply(v: ConcreteValues.Value, cc: Continuation): State = cc match {
    case AndC(_, _, cc) if v == ConcreteValues.Value.Bool(false) => Kont(v, cc)
    case AndC(first :: Nil, env, cc)                             => Step(first, env, cc)
    case AndC(first :: rest, env, cc)                            => Step(first, env, AndC(rest, env, cc))
    case AsmC(v, cc)                                             => ???
    case AssC(v, bnd, env, cc)                                   => ???
    case BegC(first :: Nil, env, cc)                             => Step(first, env, cc)
    case BegC(first :: rest, env, cc)                            => Step(first, env, BegC(rest, env, cc))
    case RefC(cc)                                                => ???
  }
}
