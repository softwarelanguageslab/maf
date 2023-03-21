package maf.language.scheme.interpreter

import maf.core.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.util.*
import maf.util.benchmarks.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.*
import scala.util.control.TailCalls.*

class KillLambdaInterpreter(cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
                            io: IO = new EmptyIO()) extends SchemeInterpreter(cb, io):

  import ConcreteValues._
  import scala.util.control.TailCalls._

  private var calls: Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]] = Map()
  def runAndIdentifyCalledLambdas(
                                   program: SchemeExp,
                                   timeout: Timeout.T,
                                   version: Version = New
                                 ): (Value, Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]]) =
    calls = Map()
    setStore(initialSto)
    (eval(program, initialEnv, timeout, version).result, calls)


  override def eval(e: SchemeExp, env: Env, timeout: Timeout.T, version: Version): TailRec[Value] =
    if timeout.reached then throw new TimeoutException()
    var caughtLambda: Option[SchemeLambda] = None
    e match
      case lambda: SchemeLambdaExp => done(Value.Clo(lambda, env))
      case call@SchemeFuncall(f, args, idn) =>
        for
          fv <- tailcall(eval(f, env, timeout, version))
          res <- fv match
            case Value.Clo(lambda@SchemeLambda(name, argsNames, body, ann, pos2), env2) =>
              caughtLambda = Some(lambda)
              if argsNames.length != args.length then
                signalException(
                  ArityError(pos2.pos, argsNames.length, args.length)
                )
              for
                argsv <- evalArgs(args, env, timeout, version)
                envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                  val addr = newAddr(AddrInfo.VarAddr(arg._1))
                  extendStore(addr, arg._2)
                  (env3 + (arg._1.name -> addr))
                }
                res <- tailcall(evalSequence(body, envExt, timeout, version))
              yield res
            case Value.Clo(lambda@SchemeVarArgLambda(name, argsNames, vararg, body, ann, pos2), env2) =>
              val arity = argsNames.length
              if args.length < arity then
                signalException(
                  VarArityError(pos2.pos, argsNames.length, args.length)
                )
              for
                argsv <- evalArgs(args, env, timeout, version)
                envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                  val addr = newAddr(AddrInfo.VarAddr(arg._1))
                  extendStore(addr, arg._2)
                  (env3 + (arg._1.name -> addr))
                }
                varArgAddr = newAddr(AddrInfo.VarAddr(vararg))
                _ = extendStore(varArgAddr, makeList(args.drop(arity).zip(argsv.drop(arity))))
                envExt2 = envExt + (vararg.name -> varArgAddr)
                res <- evalSequence(body, envExt2, timeout, version)
              yield res
            case Value.Primitive(p) =>
              for argsv <- tailcall(evalArgs(args, env, timeout, version))
                yield Primitives.allPrimitives(p).call(call, args.zip(argsv))
            case v =>
              signalException(ValueNotApplicable(v, idn))
        yield {
          caughtLambda match
            case Some(lambda) =>
              calls = calls + (lambda -> calls.getOrElse(lambda, Set()).union(Set((call, res))))
            case _ =>
          res
        }
      case SchemeIf(cond, cons, alt, _) =>
        for
          condv <- eval(cond, env, timeout, version)
          res <- condv match
            case Value.Bool(false) => tailcall(eval(alt, env, timeout, version))
            case _ => tailcall(eval(cons, env, timeout, version))
        yield res
      case SchemeLet(bindings, body, pos) =>
        tailcall(evalLet(bindings, body, pos, env, env, timeout, version))
      case SchemeLetStar(bindings, body, pos) =>
        tailcall(evalLetStar(bindings, body, pos, env, env, timeout, version))
      case SchemeLetrec(bindings, body, pos) =>
        /* First extend the environment with all bindings set to unbound */
        val envExt = bindings.foldLeft(env) { (env2, binding) =>
          val addr = newAddr(AddrInfo.VarAddr(binding._1))
          env2 + (binding._1.name -> addr)
        }
        /* Then evaluate all bindings in the extended environment */
        tailcall(evalLetrec(bindings, body, pos, envExt, env, timeout, version))
      case SchemeSet(id, v, pos) =>
        /* TODO: primitives can be reassigned with set! without being redefined */
        val addr = env.get(id.name) match
          case Some(addr) => addr
          case None => signalException(UndefinedVariableError(id))
        for
          v <- eval(v, env, timeout, version)
          _ = extendStore(addr, v)
        yield Value.Void
      case SchemeBegin(exps, _) =>
        evalSequence(exps, env, timeout, version)
      case SchemeAssert(_, _) =>
        done(Value.Void)
      case SchemeDefineVariable(_, _, _) => ???
      case SchemeVar(id) =>
        env.get(id.name) match
          case None => signalException(UndefinedVariableError(id))
          case Some(addr) =>
            lookupStoreOption(addr) match
              case None => signalException(UninitialisedVariableError(id))
              case Some(v) => done(v)
      case SchemeValue(v, _) =>
        done(evalLiteral(v, e))
      case CSchemeFork(body, _) =>
        // TODO: This is a bit hacky in terms of tailcalls
        done(Value.Thread(safeFuture(eval(body, env, timeout, version).result)))
      case CSchemeJoin(tExp, _) =>
        for
          threadv <- eval(tExp, env, timeout, version)
          res <- threadv match
            case Value.Thread(fut) =>
              done(Await.result(fut, timeout.timeLeft.map(Duration(_, TimeUnit.NANOSECONDS)).getOrElse(Duration.Inf)))
            case v => signalException(TypeError("Join expected thread", v))
        yield res
      case SchemeCodeChange(old, nw, _) =>
        if version == Old then tailcall(eval(old, env, timeout, version)) else tailcall(eval(nw, env, timeout, version))
      case _ => throw new Exception(s"Unsupported Scheme expression: $e")
