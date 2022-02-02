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
 * This is an interpreter that runs a program and calls a callback at every evaluated value. This interpreter dictates the concrete semantics of the
 * Scheme language analyzed by MAF.
 */
class SchemeInterpreter(
    cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[TailRec[ConcreteValues.Value]]
    with ConcreteSchemePrimitives:

    import ConcreteValues._

    import scala.util.control.TailCalls._

    /** Evaluates `program`. Will check the analysis result by calling `compare` on all encountered values. */
    def run(
        program: SchemeExp,
        timeout: Timeout.T,
        version: Version = New
      ): Value =
        setStore(initialSto)
        eval(program, initialEnv, timeout, version).result

    def safeFuture(bdy: => Value): Future[Value] = Future {
      try bdy
      catch
          case e: VirtualMachineError =>
            throw ChildThreadDiedException(e)
    }

    // Access to cb should be synchronized on 'Callback'.
    object Callback:
        def call(i: Identity, v: Value): Unit =
          synchronized {
            cb(i, v)
          }

    // Keep an artificial call stack to ease debugging.
    var callStack: List[String] = List()

    // TODO: The stack mechanism might have been broken due to the use of TailRec
    // TODO: This may not work with concurrent programs at all, but at least we make it thread-safe and avoid exceptions.
    def stackedCall(
        name: Option[String],
        idn: Identity,
        block: => TailRec[Value]
      ): TailRec[Value] =
      synchronized {
        val n = name.getOrElse("λ") + s"@${idn.pos}"
        if stack then callStack = n :: callStack
        val res = block
        if stack then
            callStack match
                case Nil => System.err.nn.println("The call stack tracking does currently not work correctly with concurrent programs.")
                case _   => callStack = callStack.tail
        res
      }

    override def signalException[R](msg: String): R =
        val m = if stack then callStack.mkString(s"$msg\n Callstack:\n * ", "\n * ", "\n **********") else msg
        super.signalException(m)

    def evalSequence(
        exps: List[SchemeExp],
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
      exps match
          case Nil      => done(Value.Void)
          case e :: Nil => tailcall(eval(e, env, timeout, version))
          case e1 :: exps =>
            for
                v1 <- tailcall(eval(e1, env, timeout, version))
                vn <- tailcall(evalSequence(exps, env, timeout, version))
            yield vn

    def evalLet(
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        pos: Identity,
        envExt: Env,
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
      bindings match
          case Nil => tailcall(eval(SchemeBegin(body, pos), envExt, timeout, version))
          case binding :: bindings =>
            val addr = newAddr(AddrInfo.VarAddr(binding._1))
            for
                bindingv <- tailcall(eval(binding._2, env, timeout, version))
                _ = extendStore(addr, bindingv)
                res <- tailcall(evalLet(bindings, body, pos, envExt + (binding._1.name -> addr), env, timeout, version))
            yield res

    def evalLetStar(
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        pos: Identity,
        envExt: Env,
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
      bindings match
          case Nil => tailcall(eval(SchemeBegin(body, pos), envExt, timeout, version))
          case binding :: bindings =>
            val addr = newAddr(AddrInfo.VarAddr(binding._1))
            for
                bindingv <- tailcall(eval(binding._2, envExt, timeout, version))
                _ = extendStore(addr, bindingv)
                res <- tailcall(evalLetStar(bindings, body, pos, envExt + (binding._1.name -> addr), env, timeout, version))
            yield res

    def evalLetrec(
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        pos: Identity,
        envExt: Env,
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
      bindings match
          case Nil => tailcall(eval(SchemeBegin(body, pos), envExt, timeout, version))
          case binding :: bindings =>
            for
                bindingv <- tailcall(eval(binding._2, envExt, timeout, version))
                _ = extendStore(envExt(binding._1.name), bindingv)
                res <- tailcall(evalLetrec(bindings, body, pos, envExt, env, timeout, version))
            yield res

    //determines whether a given value should be checked or not
    private def checkValue(v: Value): Boolean = v match
        case Value.Undefined(_) =>
          //println(s"Undefined behavior arising from identity $idn seen at ${e.idn.pos}")
          false
        case _ =>
          true

    private def checkAddr(a: Addr): Boolean = a._2 match
        case _: AddrInfo.VarAddr | _: AddrInfo.PtrAddr => true
        case _                                         => false

    override def extendStore(a: Addr, v: Value): Unit =
        if checkAddr(a) && checkValue(v) then Callback.call(a._2.idn, v)
        super.extendStore(a, v)

    def evalArgs(
        args: List[SchemeExp],
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[List[Value]] =
      args match
          case Nil => done(Nil)
          case arg :: args =>
            for
                argv <- tailcall(eval(arg, env, timeout, version))
                argsv <- tailcall(evalArgs(args, env, timeout, version))
            yield argv :: argsv

    protected def isProcedure(v: Value): Boolean = v match
        case _: Value.Primitive | _: Value.Clo => true
        case _                                 => false

    protected def applyFun(
        f: Value,
        call: SchemeFuncall,
        argsv: List[Value],
        args: List[SchemeExp],
        idn: Identity,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
      f match
          // A regular closure with a fixed amount of parameters
          case Value.Clo(lambda @ SchemeLambda(name, argsNames, body, ann, pos2), env2) =>
            if argsNames.length != argsv.length then
                signalException(
                  s"Invalid function call at position ${idn}: ${argsv.length} arguments given to function lambda (${lambda.idn.pos}), while exactly ${argsNames.length} are expected."
                )
            for
                _ <- done(())
                envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                    val addr = newAddr(AddrInfo.VarAddr(arg._1))
                    extendStore(addr, arg._2)
                    (env3 + (arg._1.name -> addr))
                }
                ret <- stackedCall(name, pos2, tailcall(eval(SchemeBody(body), envExt, timeout, version)))
            yield ret

          // A closure with a variable amount of parameters
          case Value.Clo(lambda @ SchemeVarArgLambda(name, argsNames, vararg, body, ann, pos2), env2) =>
            val arity = argsNames.length
            if argsv.length < arity then
                signalException(
                  s"Invalid function call at position $idn: ${args.length} arguments given, while at least ${argsNames.length} are expected."
                )
            for
                _ <- done(())
                envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                    val addr = newAddr(AddrInfo.VarAddr(arg._1))
                    extendStore(addr, arg._2)
                    (env3 + (arg._1.name -> addr))
                }
                varArgAddr = newAddr(AddrInfo.VarAddr(vararg))
                _ = extendStore(varArgAddr, makeList(args.drop(arity).zip(argsv.drop(arity))))
                envExt2 = envExt + (vararg.name -> varArgAddr)
                ret <- stackedCall(name, pos2, eval(SchemeBody(body), envExt2, timeout, version))
            yield ret

          case Value.Primitive(p) =>
            tailcall(
              stackedCall(Some(p), Identity.none, tailcall(done(Primitives.allPrimitives(p).call(call, args.zip(argsv)))))
            )
          case v =>
            signalException(s"Invalid function call at position ${idn}: ${v} is not a closure or a primitive.")

    def eval(
        e: SchemeExp,
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
        if timeout.reached then throw new TimeoutException()
        e match
            case lambda: SchemeLambdaExp => done(Value.Clo(lambda, env))
            case call @ SchemeFuncall(f, args, idn) =>
              for
                  fv <- tailcall(eval(f, env, timeout, version))
                  res <- fv match
                      case Value.Clo(lambda @ SchemeLambda(name, argsNames, body, ann, pos2), env2) =>
                        if argsNames.length != args.length then
                            signalException(
                              s"Invalid function call at position ${idn}: ${args.length} arguments given to function lambda (${lambda.idn.pos}), while exactly ${argsNames.length} are expected."
                            )
                        for
                            argsv <- evalArgs(args, env, timeout, version)
                            envExt = argsNames.zip(argsv).foldLeft(env2) { (env3, arg) =>
                                val addr = newAddr(AddrInfo.VarAddr(arg._1))
                                extendStore(addr, arg._2)
                                (env3 + (arg._1.name -> addr))
                            }
                            res <- stackedCall(name, pos2, tailcall(eval(SchemeBody(body), envExt, timeout, version)))
                        yield res
                      case Value.Clo(lambda @ SchemeVarArgLambda(name, argsNames, vararg, body, ann, pos2), env2) =>
                        val arity = argsNames.length
                        if args.length < arity then
                            signalException(
                              s"Invalid function call at position $idn: ${args.length} arguments given, while at least ${argsNames.length} are expected."
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
                            res <- stackedCall(name, pos2, eval(SchemeBody(body), envExt2, timeout, version))
                        yield res
                      case Value.Primitive(p) =>
                        tailcall(
                          stackedCall(Some(p),
                                      Identity.none,
                                      for argsv <- tailcall(evalArgs(args, env, timeout, version))
                                      yield Primitives.allPrimitives(p).call(call, args.zip(argsv))
                          )
                        )
                      case v =>
                        signalException(s"Invalid function call at position ${idn}: ${v} is not a closure or a primitive.")
              yield res
            case SchemeIf(cond, cons, alt, _) =>
              for
                  condv <- eval(cond, env, timeout, version)
                  res <- condv match
                      case Value.Bool(false) => tailcall(eval(alt, env, timeout, version))
                      case _                 => tailcall(eval(cons, env, timeout, version))
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
                  case None       => signalException(s"Undefined variable $id accessed at position $pos")
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
                  case None => signalException(s"Undefined variable $id at position ${id.idn}.")
                  case Some(addr) =>
                    lookupStoreOption(addr) match
                        case None    => signalException(s"Uninitialised variable $id at position ${id.idn}.")
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
                      case v => signalException(s"Join expected thread, but got $v")
              yield res
            case SchemeCodeChange(old, nw, _) =>
              if version == Old then tailcall(eval(old, env, timeout, version)) else tailcall(eval(nw, env, timeout, version))
            case _ => throw new Exception(s"Unsupported Scheme expression: $e")

object SchemeInterpreter:

    import maf.language.scheme.primitives._

    import scala.concurrent.duration._

    val timeout: FiniteDuration = Duration(30, SECONDS)

    def main(args: Array[String]): Unit =
      if args.length == 1 then
          val text = Reader.loadFile(args(0))
          val pgm = SchemeUndefiner.undefine(SchemePrelude.addPrelude(SchemeParser.parse(text)))
          val interpreter = new SchemeInterpreter((id, v) => ())
          val res = interpreter.run(pgm, Timeout.start(timeout))
          println(s"Result: $res")
      else println(s"Expected file to run as argument")
