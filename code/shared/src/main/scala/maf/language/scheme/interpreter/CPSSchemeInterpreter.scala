package maf.language.scheme.interpreter

import maf.core._
import maf.language.change.CodeVersion.{New, Old, Version}
import maf.language.scheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.util.benchmarks.Timeout

import scala.concurrent.TimeoutException

class CPSSchemeInterpreter(
    cb: (Identity, Value) => Unit = (_, _) => (), // TODO: incoropate
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[Value]
       with ConcreteSchemePrimitives {

  // TODO: change for multi-threaded programs.
  override def run(
      program: SchemeExp,
      timeout: Timeout.T,
      version: Version
    ): Value = {
    setStore(initialSto)
    var state: State = Step(program, initialEnv, EndC())
    try {
      while (!timeout.reached)
        state = state match {
          case Step(exp, env, cc) => eval(exp, env, version, cc)
          case Kont(v, EndC(_))   => return v
          case Kont(_, TrdC(_))   => ???
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

  case class ArgC(
      exps: List[SchemeExp],
      env: Env,
      f: Value,
      argv: List[Value],
      call: SchemeFuncall,
      cc: Continuation)
      extends Continuation

  case class BegC(
      exps: List[SchemeExp],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class CdrC(
      cdr: SchemeExp,
      env: Env,
      e: SchemeExp,
      cc: Continuation)
      extends Continuation

  case class EndC(cc: Continuation = EndC()) extends Continuation // End of the program.

  case class FunC(
      args: List[SchemeExp],
      env: Env,
      call: SchemeFuncall,
      cc: Continuation)
      extends Continuation

  case class IffC(
      cons: SchemeExp,
      alt: SchemeExp,
      env: Env,
      cc: Continuation)
      extends Continuation

  case class LetC(
      bnd: List[(Identifier, SchemeExp)],
      env: Env,
      bvals: List[Value],
      let: SchemeLet,
      cc: Continuation)
      extends Continuation

  case class LtsC(
      i: Identifier,
      bnd: List[(Identifier, SchemeExp)],
      env: Env,
      let: SchemeLetStar,
      cc: Continuation)
      extends Continuation

  case class OrrC(
      exps: List[SchemeExp],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class PaiC(
      car: Value,
      e: SchemeExp,
      cc: Continuation)
      extends Continuation

  case class RetC(addr: Addr, cc: Continuation) extends Continuation // Allows to write the return value of a function call to the store.
  case class SetC(addr: Addr, cc: Continuation) extends Continuation

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

  case class Kont(v: Value, cc: Continuation) extends State

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
    case call @ SchemeFuncall(f, args, _)              => Step(f, env, FunC(args, env, call, cc))
    case SchemeIf(cond, cons, alt, _)                  => Step(cond, env, IffC(cons, alt, env, cc))
    case lambda: SchemeLambdaExp                       => Kont(Value.Clo(lambda, env), cc)
    case SchemeLet(Nil, body, pos)                     => Step(SchemeBegin(body, pos), env, cc)
    case let @ SchemeLet((_, e) :: rest, _, _)         => Step(e, env, LetC(rest, env, Nil, let, cc))
    case SchemeLetStar(Nil, body, pos)                 => Step(SchemeBegin(body, pos), env, cc)
    case let @ SchemeLetStar((i, e) :: rest, _, _)     => Step(e, env, LtsC(i, rest, env, let, cc))
    case SchemeLetrec(Nil, body, pos)                  => Step(SchemeBegin(body, pos), env, cc)
    case SchemeLetrec(bindings, body, _)               => ???
    case SchemeNamedLet(name, bindings, body, _)       => ???
    case SchemeOr(Nil, _)                              => Kont(Value.Bool(false), cc)
    case SchemeOr(first :: Nil, _)                     => Step(first, env, cc)
    case SchemeOr(first :: rest, _)                    => Step(first, env, OrrC(rest, env, cc))
    case SchemePair(car, cdr, _)                       => Step(car, env, CdrC(cdr, env, exp, cc))
    case SchemeSet(variable, value, _) =>
      env.get(variable.name) match {
        case Some(addr) => Step(value, env, SetC(addr, cc))
        case None       => stackedException(s"Unbound variable $variable at position ${variable.idn}.")
      }
    case SchemeSetLex(_, _, _, _)   => stackedException("Unsupported: lexical addresses.")
    case SchemeSplicedPair(_, _, _) => stackedException("NYI -- Unquote splicing")
    case SchemeValue(value, _)      => Kont(evalLiteral(value, exp), cc)
    case SchemeVar(id) =>
      env.get(id.name).flatMap(lookupStoreOption).map(Kont(_, cc)).getOrElse(stackedException(s"Unbound variable $id at position ${id.idn}."))
    case SchemeVarLex(_, _) => stackedException("Unsupported: lexical addresses.")

    case CSchemeFork(body, _) => ???
    case CSchemeJoin(tExp, _) => ???

    case _ => throw new Exception(s"Unsupported expression type: ${exp.label}.")
  }

  def apply(v: Value, cc: Continuation): State = cc match {
    case AndC(_, _, cc) if v == Value.Bool(false)        => Kont(v, cc)
    case AndC(first :: Nil, env, cc)                     => Step(first, env, cc)
    case AndC(first :: rest, env, cc)                    => Step(first, env, AndC(rest, env, cc))
    case ArgC(Nil, _, f, argv, call, cc)                 => continueBody(f, v :: argv, call, cc)
    case ArgC(first :: rest, env, f, argv, call, cc)     => Step(first, env, ArgC(rest, env, f, v :: argv, call, cc))
    case BegC(first :: Nil, env, cc)                     => Step(first, env, cc)
    case BegC(first :: rest, env, cc)                    => Step(first, env, BegC(rest, env, cc))
    case CdrC(cdr, env, exp, cc)                         => Step(cdr, env, PaiC(v, exp, cc))
    case FunC(args, env, call, cc)                       => continueArgs(v, args, env, call, cc)
    case IffC(_, alt, env, cc) if v == Value.Bool(false) => Step(alt, env, cc)
    case IffC(cons, _, env, cc)                          => Step(cons, env, cc)
    case LetC(Nil, env, bvals, let, cc)                  => Step(SchemeBegin(let.body, let.idn), extendEnv(let.bindings.map(_._1), (v :: bvals).reverse, env), cc)
    case LetC((_, e) :: rest, env, bvals, let, cc)       => Step(e, env, LetC(rest, env, v :: bvals, let, cc))
    case LtsC(i, Nil, env, let, cc)                      => Step(SchemeBegin(let.body, let.idn), extendEnv(env, (i, v)), cc)
    case LtsC(i1, (i2, e) :: rest, env, let, cc)         => val extEnv = extendEnv(env, (i1, v)); Step(e, extEnv, LtsC(i2, rest, extEnv, let, cc))
    case OrrC(_, _, cc) if v != Value.Bool(false)        => Kont(v, cc)
    case OrrC(first :: Nil, env, cc)                     => Step(first, env, cc)
    case OrrC(first :: rest, env, cc)                    => Step(first, env, OrrC(rest, env, cc))
    case PaiC(car, e, cc)                                => Kont(allocateCons(e, car, v), cc)
    case RetC(addr, cc)                                  => extendStore(addr, v); Kont(v, cc)
    case SetC(addr, cc)                                  => extendStore(addr, v); Kont(Value.Void, cc)
  }

  // Evaluate the arguments of a function.
  def continueArgs(
      f: Value,
      args: List[SchemeExp],
      env: Env,
      call: SchemeFuncall,
      cc: Continuation
    ): State = {
    // First, check the arity.
    f match {
      case Value.Clo(lambda @ SchemeLambda(argNames, _, _), _, name) =>
        if (args.length != argNames.length)
          stackedException(s"Invalid function call at position ${call.idn}: ${args.length} arguments given to function ${name
            .getOrElse("lambda")} (${lambda.idn.pos}), while exactly ${argNames.length} are expected.")
      case Value.Clo(lambda @ SchemeVarArgLambda(argNames, _, _, _), _, name) =>
        if (args.length < argNames.length)
          stackedException(s"Invalid function call at position ${call.idn}: ${args.length} arguments given to function ${name
            .getOrElse("lambda")} (${lambda.idn.pos}), while at least ${argNames.length} are expected.")
      case Value.Primitive(_) => // Arity is checked upon primitive call.
      case v                  => stackedException(s"Invalid function call at position ${call.idn}: ${v} is not a closure or a primitive.")
    }
    // If the arity is ok, evaluate the arguments.
    args match {
      case Nil           => continueBody(f, Nil, call, cc)
      case first :: rest => Step(first, env, ArgC(rest, env, f, Nil, call, cc))
    }
  }

  // Evaluate the body of a function.
  def continueBody(
      f: Value,
      argvsRev: List[Value],
      call: SchemeFuncall,
      cc: Continuation
    ): State = {
    val argvs = argvsRev.reverse
    f match {
      case Value.Clo(SchemeLambda(argNames, body, _), env2, _) =>
        Step(SchemeBody(body), extendEnv(argNames, argvs, env2), RetC(newAddr(AddrInfo.RetAddr(SchemeBody(body))), cc))
      case Value.Clo(SchemeVarArgLambda(argNames, vararg, body, _), env2, _) =>
        val varArgAddr = newAddr(AddrInfo.VarAddr(vararg))
        extendStore(varArgAddr, makeList(call.args.drop(argNames.length).zip(argvs.drop(argNames.length))))
        val envExt = extendEnv(argNames, argvs, env2) + (vararg.name -> varArgAddr)
        Step(SchemeBody(body), envExt, RetC(newAddr(AddrInfo.RetAddr(SchemeBody(body))), cc))
      case Value.Primitive(p) => Kont(Primitives.allPrimitives(p).call(call, call.args.zip(argvs)), cc)
    }
  }

  def extendEnv(
      ids: List[Identifier],
      values: List[Value],
      env: Env
    ): Env = ids.zip(values).foldLeft(env)(extendEnv)

  def extendEnv(env: Env, idv: (Identifier, Value)): Env = {
    val addr = newAddr(AddrInfo.VarAddr(idv._1))
    extendStore(addr, idv._2)
    env + (idv._1.name -> addr)
  }
}
