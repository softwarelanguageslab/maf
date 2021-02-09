package maf.language.scheme.interpreter

import maf.core._
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.util._
import maf.util.benchmarks.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._
import scala.util.control.TailCalls._

case class ChildThreadDiedException(e: VirtualMachineError) extends Exception(s"A child thread has tragically died with ${e.getMessage}.")

case class UnexpectedValueTypeException[V](v: V) extends Exception(s"The interpreter encountered an unexpected value during its execution: $v.")

/**
 * This is an interpreter that runs a program and calls a callback at every evaluated value.
 * This interpreter dictates the concrete semantics of the Scheme language analyzed by MAF.
 */
class SchemeInterpreter(
    cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[TailRec[ConcreteValues.Value]]
       with ConcreteSchemePrimitives {

  import ConcreteValues._

  import scala.util.control.TailCalls._

  /**
   * Evaluates `program`.
   * Will check the analysis result by calling `compare` on all encountered values.
   */
  def run(
      program: SchemeExp,
      timeout: Timeout.T,
      version: Version = New
    ): Value = {
    setStore(initialSto)
    val res = eval(program, initialEnv, timeout, version).result
    val resAddr = newAddr(AddrInfo.RetAddr(program))
    extendStore(resAddr, res)
    res
  }

  def safeFuture(bdy: => Value): Future[Value] = Future {
    try bdy
    catch {
      case e: VirtualMachineError =>
        throw ChildThreadDiedException(e)
    }
  }

  // Access to cb should be synchronized on 'Callback'.
  object Callback {
    def call(i: Identity, v: Value): Unit = synchronized {
      cb(i, v)
    }
  }

  var compared = 0

  def check(i: Identity, v: Value): Value = {
    compared += 1
    v match {
      case Value.Undefined(idn @ _) => () // println(s"Undefined behavior arising from identity $idn seen at ${e.idn.pos}")
      case Value.Unbound(idn)       => println(s"Seen unbound identifier $idn at ${i}")
      case _                        => ()
    }
    Callback.call(i, v)
    v
  }

  // Keep an artificial call stack to ease debugging.
  var callStack: List[String] = List()

  // TODO: The stack mechanism might have been broken due to the use of TailRec
  // TODO: This may not work with concurrent programs at all, but at least we make it thread-safe and avoid exceptions.
  def stackedCall(
      name: Option[String],
      idn: Identity,
      block: => TailRec[Value]
    ): TailRec[Value] = synchronized {
    val n = name.getOrElse("λ") + s"@${idn.pos}"
    if (stack) callStack = n :: callStack
    val res = block
    if (stack) callStack match {
      case Nil => System.err.println("The call stack tracking does currently not work correctly with concurrent programs.")
      case _   => callStack = callStack.tail
    }
    res
  }

  def stackedException[R](msg: String): R = {
    val m = if (stack) callStack.mkString(s"$msg\n Callstack:\n * ", "\n * ", "\n **********") else msg
    throw new Exception(m)
  }

  def evalSequence(
      exps: List[SchemeExp],
      env: Env,
      timeout: Timeout.T,
      version: Version
    ): TailRec[Value] =
    exps match {
      case Nil      => done(Value.Void)
      case e :: Nil => tailcall(eval(e, env, timeout, version))
      case e1 :: exps =>
        for {
          v1 <- tailcall(eval(e1, env, timeout, version))
          vn <- tailcall(evalSequence(exps, env, timeout, version))
        } yield vn
    }

  def evalLet(
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      pos: Identity,
      envExt: Env,
      env: Env,
      timeout: Timeout.T,
      version: Version
    ): TailRec[Value] =
    bindings match {
      case Nil => tailcall(eval(SchemeBegin(body, pos), envExt, timeout, version))
      case binding :: bindings =>
        val addr = newAddr(AddrInfo.VarAddr(binding._1))
        for {
          bindingv <- tailcall(eval(binding._2, env, timeout, version))
          _ = extendStore(addr, check(binding._1.idn, bindingv))
          res <- tailcall(evalLet(bindings, body, pos, envExt + (binding._1.name -> addr), env, timeout, version))
        } yield res
    }

  def evalLetStar(
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      pos: Identity,
      envExt: Env,
      env: Env,
      timeout: Timeout.T,
      version: Version
    ): TailRec[Value] =
    bindings match {
      case Nil => tailcall(eval(SchemeBegin(body, pos), envExt, timeout, version))
      case binding :: bindings =>
        val addr = newAddr(AddrInfo.VarAddr(binding._1))
        for {
          bindingv <- tailcall(eval(binding._2, envExt, timeout, version))
          _ = extendStore(addr, check(binding._1.idn, bindingv))
          res <- tailcall(evalLetStar(bindings, body, pos, envExt + (binding._1.name -> addr), env, timeout, version))
        } yield res
    }

  def evalLetrec(
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      pos: Identity,
      envExt: Env,
      env: Env,
      timeout: Timeout.T,
      version: Version
    ): TailRec[Value] =
    bindings match {
      case Nil => tailcall(eval(SchemeBegin(body, pos), envExt, timeout, version))
      case binding :: bindings =>
        for {
          bindingv <- tailcall(eval(binding._2, envExt, timeout, version))
          namedValue = bindingv match {
            case Value.Clo(lambda, env, _) => Value.Clo(lambda, env, Some(binding._1.name)) // Add names to closures.
            case _                         => bindingv
          }
          _ = extendStore(envExt(binding._1.name), check(binding._1.idn, namedValue))
          res <- tailcall(evalLetrec(bindings, body, pos, envExt, env, timeout, version))
        } yield res
    }

  def evalArgs(
      args: List[SchemeExp],
      env: Env,
      timeout: Timeout.T,
      version: Version
    ): TailRec[List[Value]] =
    args match {
      case Nil => done(Nil)
      case arg :: args =>
        for {
          argv <- tailcall(eval(arg, env, timeout, version))
          argsv <- tailcall(evalArgs(args, env, timeout, version))
        } yield argv :: argsv
    }

  def eval(
      e: SchemeExp,
      env: Env,
      timeout: Timeout.T,
      version: Version
    ): TailRec[Value] = {
    if (timeout.reached) throw new TimeoutException()
    e match {
      case lambda: SchemeLambdaExp => done(Value.Clo(lambda, env))
      case call @ SchemeFuncall(f, args, idn) =>
        for {
          fv <- tailcall(eval(f, env, timeout, version))
          res <- fv match {
            case Value.Clo(lambda @ SchemeLambda(argsNames, body, pos2), env2, name) =>
              if (argsNames.length != args.length) {
                stackedException(
                  s"Invalid function call at position ${idn}: ${args.length} arguments given to function lambda (${lambda.idn.pos}), while exactly ${argsNames.length} are expected."
                )
              }
              for {
                argsv <- evalArgs(args, env, timeout, version)
                envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                  val addr = newAddr(AddrInfo.VarAddr(arg._1))
                  extendStore(addr, check(arg._1.idn, arg._2))
                  (env3 + (arg._1.name -> addr))
                }
                res <- stackedCall(name, pos2, tailcall(eval(SchemeBegin(body, pos2), envExt, timeout, version)))
                resAddr = newAddr(AddrInfo.RetAddr(SchemeBody(lambda.body)))
                _ = extendStore(resAddr, res)
              } yield res
            case Value.Clo(lambda @ SchemeVarArgLambda(argsNames, vararg, body, pos2), env2, name) =>
              val arity = argsNames.length
              if (args.length < arity) {
                stackedException(
                  s"Invalid function call at position $idn: ${args.length} arguments given, while at least ${argsNames.length} are expected."
                )
              }
              for {
                argsv <- evalArgs(args, env, timeout, version)
                envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                  val addr = newAddr(AddrInfo.VarAddr(arg._1))
                  extendStore(addr, check(arg._1.idn, arg._2))
                  (env3 + (arg._1.name -> addr))
                }
                varArgVals <- evalArgs(args.drop(arity), env, timeout, version)
                varArgAddr = newAddr(AddrInfo.VarAddr(vararg))
                _ = extendStore(varArgAddr, makeList(args.zip(varArgVals)))
                envExt2 = envExt + (vararg.name -> varArgAddr)
                res <- stackedCall(name, pos2, eval(SchemeBegin(body, pos2), envExt2, timeout, version))
                resAddr = newAddr(AddrInfo.RetAddr(SchemeBody(lambda.body)))
                _ = extendStore(resAddr, res)
              } yield res
            case Value.Primitive(p) =>
              tailcall(
                stackedCall(Some(p),
                            Identity.none,
                            for {
                              argsv <- tailcall(evalArgs(args, env, timeout, version))
                            } yield Primitives.allPrimitives(p).call(call, args.zip(argsv))
                )
              )
            case v =>
              stackedException(s"Invalid function call at position ${idn}: ${v} is not a closure or a primitive.")
          }
        } yield res
      case SchemeIf(cond, cons, alt, _) =>
        for {
          condv <- eval(cond, env, timeout, version)
          res <- condv match {
            case Value.Bool(false) => tailcall(eval(alt, env, timeout, version))
            case _                 => tailcall(eval(cons, env, timeout, version))
          }
        } yield res
      case SchemeLet(bindings, body, pos) =>
        tailcall(evalLet(bindings, body, pos, env, env, timeout, version))
      case SchemeLetStar(bindings, body, pos) =>
        tailcall(evalLetStar(bindings, body, pos, env, env, timeout, version))
      case SchemeLetrec(bindings, body, pos) =>
        /* First extend the environment with all bindings set to unbound */
        val envExt = bindings.foldLeft(env) { (env2, binding) =>
          val addr = newAddr(AddrInfo.VarAddr(binding._1))
          extendStore(addr, Value.Unbound(binding._1))
          val env3 = env2 + (binding._1.name -> addr)
          env3
        }
        /* Then evaluate all bindings in the extended environment */
        tailcall(evalLetrec(bindings, body, pos, envExt, env, timeout, version))
      case SchemeNamedLet(name, bindings, body, pos) =>
        val addr = newAddr(AddrInfo.VarAddr(name))
        val env2 = env + (name.name -> addr)
        val (prs, ags) = bindings.unzip
        val lambda = SchemeLambda(prs, body, pos)
        val clo = Value.Clo(lambda, env2, Some(name.name))
        extendStore(addr, clo)
        tailcall(eval(SchemeFuncall(lambda, ags, pos), env2, timeout, version))
      case SchemeSet(id, v, pos) =>
        /* TODO: primitives can be reassigned with set! without being redefined */
        val addr = env.get(id.name) match {
          case Some(addr) => addr
          case None       => stackedException(s"Unbound variable $id accessed at position $pos")
        }
        for {
          v <- eval(v, env, timeout, version)
          _ = extendStore(addr, v)
        } yield Value.Void
      case SchemeBegin(exps, _) =>
        evalSequence(exps, env, timeout, version)
      case SchemeAnd(Nil, _) =>
        done(Value.Bool(true))
      case SchemeAnd(e :: Nil, _) =>
        tailcall(eval(e, env, timeout, version))
      case SchemeAnd(e :: exps, pos) =>
        for {
          v1 <- eval(e, env, timeout, version)
          v2 <- v1 match {
            case Value.Bool(false) => done(Value.Bool(false))
            case _                 => tailcall(eval(SchemeAnd(exps, pos), env, timeout, version))
          }
        } yield v2
      case SchemeOr(Nil, _) =>
        done(Value.Bool(false))
      case SchemeOr(e :: exps, pos) =>
        for {
          v1 <- tailcall(eval(e, env, timeout, version))
          v2 <- v1 match {
            case Value.Bool(false) => tailcall(eval(SchemeOr(exps, pos), env, timeout, version))
            case v                 => done(v)
          }
        } yield v2
      case SchemeAssert(_, _) =>
        done(Value.Void)
      case SchemeDefineVariable(_, _, _)             => ???
      case SchemeDefineFunction(_, _, _, _)          => ???
      case SchemeDefineVarArgFunction(_, _, _, _, _) => ???
      case SchemeVar(id) =>
        env.get(id.name) match {
          case Some(addr) =>
            lookupStoreOption(addr) match {
              case Some(v) => done(v)
              case None    => stackedException(s"Unbound variable $id at position ${id.idn}.")
            }
          case None => stackedException(s"Undefined variable $id at position ${id.idn}.")
        }
      case SchemePair(car, cdr, _) =>
        for {
          carv <- eval(car, env, timeout, version)
          cdrv <- eval(cdr, env, timeout, version)
        } yield allocateCons(e, carv, cdrv)
      case SchemeSplicedPair(_, _, _) =>
        stackedException("NYI -- Unquote splicing")
      //val splicev = eval(splice,env,timeout)
      //val cdrv    = eval(cdr,env,timeout)
      //Primitives.Append.append(splicev,cdrv)
      case SchemeValue(v, _) =>
        done(evalLiteral(v, e))
      case CSchemeFork(body, _) =>
        // TODO: This is a bit hacky in terms of tailcalls
        done(Value.Thread(safeFuture(eval(body, env, timeout, version).result)))
      case CSchemeJoin(tExp, _) =>
        for {
          threadv <- eval(tExp, env, timeout, version)
          res <- threadv match {
            case Value.Thread(fut) => done(Await.result(fut, timeout.timeLeft.map(Duration(_, TimeUnit.NANOSECONDS)).getOrElse(Duration.Inf)))
            case v                 => stackedException(s"Join expected thread, but got $v")
          }
        } yield res
      case SchemeCodeChange(old, nw, _) =>
        if (version == Old) tailcall(eval(old, env, timeout, version)) else tailcall(eval(nw, env, timeout, version))
    }
  }
}

object SchemeInterpreter {

  import maf.language.scheme.primitives._

  import scala.concurrent.duration._

  val timeout: FiniteDuration = Duration(30, SECONDS)

  def main(args: Array[String]): Unit =
    if (args.length == 1) {
      val text = Reader.loadFile(args(0))
      val pgm = SchemeUndefiner.undefine(List(SchemePrelude.addPrelude(SchemeParser.parse(text), Set("newline", "display"))))
      val interpreter = new SchemeInterpreter((id, v) => ())
      val res = interpreter.run(pgm, Timeout.start(timeout))
      println(s"Result: $res")
    } else {
      println(s"Expected file to run as argument")
    }
}
