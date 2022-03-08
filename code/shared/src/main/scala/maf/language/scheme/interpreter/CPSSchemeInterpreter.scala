package maf.language.scheme.interpreter

import maf.core._
import maf.language.change.CodeVersion.{New, Old, Version}
import maf.language.scheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.util.benchmarks.Timeout

import scala.collection.immutable.Queue
import scala.concurrent.TimeoutException

class CPSSchemeInterpreter(
    cb: (Identity, Value) => Unit = (_, _) => (), // TODO: incorporate
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[Value]
    with ConcreteSchemePrimitives:

    type TID = Int

    // Note that these variables make concurrent uses of `run` not thread safe.
    var work: Queue[State] = _ // Round-robin scheduling.
    var waiting: Map[TID, Either[Set[Continuation], Value]] = _
    var state: State = _
    var steps: TID = _
    var tid = 0

    def scheduleNext(): Unit =
        steps = scala.util.Random.nextInt(10)
        val (s, w) = work.dequeue
        state = s
        work = w

    def allocTID(): TID =
        tid = tid + 1
        tid

    override def run(
        program: SchemeExp,
        timeout: Timeout.T,
        version: Version
      ): Value =
        work = Queue()
        waiting = Map().withDefaultValue(Left(Set()))
        setStore(initialSto)
        work = work.enqueue(Step(program, initialEnv, EndC()))
        scheduleNext()
        try
            while !timeout.reached do
                // Let every thread perform some steps.
                if steps == 0 then
                    work = work.enqueue(state)
                    scheduleNext()
                steps = steps - 1
                state = state match
                    case Step(exp, env, cc) => eval(exp, env, version, cc)
                    case Kont(v, EndC())    => return v
                    case Kont(v, TrdC(tid)) =>
                      val blocked = waiting(tid)
                      waiting = waiting + (tid -> Right(v))
                      blocked match
                          case Left(continuations) => work = work.enqueueAll(continuations.map(Kont(v, _)))
                          case Right(_)            => throw new Exception(s"Thread with $tid already finished.")
                      scheduleNext()
                      state
                    case Kont(v, cc) => apply(v, cc)
            throw new TimeoutException()
        catch
            // Use the continuations to print the stack trace.
            case ProgramError(message) =>
              var cc = state.cc
              var msg: String = s"$message\n Callstack:"
              if stack then
                  while
                      msg = msg + s"\n * ${cc.toString}"
                      cc = cc.asInstanceOf[InternalContinuation].cc
                      cc != EndC() && cc != TrdC()
                  do ()
                  msg += "\n **********"
              throw new Exception(msg)

    /* ************************* */
    /* ***** CONTINUATIONS ***** */
    /* ************************* */

    sealed trait Continuation

    trait InternalContinuation extends Continuation:
        val cc: Continuation

    case class ArgC(
        exps: List[SchemeExp],
        env: Env,
        f: Value,
        argv: List[Value],
        call: SchemeFuncall,
        cc: Continuation)
        extends InternalContinuation

    case class BegC(exps: List[SchemeExp], env: Env, cc: Continuation) extends InternalContinuation

    case class EndC() extends Continuation // End of the program.
    case class FunC(args: List[SchemeExp], env: Env, call: SchemeFuncall, cc: Continuation) extends InternalContinuation

    case class IffC(cons: SchemeExp, alt: SchemeExp, env: Env, cc: Continuation) extends InternalContinuation

    case class JoiC(cc: Continuation) extends InternalContinuation

    case class LetC(bnd: List[(Identifier, SchemeExp)], env: Env, bvals: List[Value], let: SchemeLet, cc: Continuation) extends InternalContinuation

    case class LtsC(i: Identifier, bnd: List[(Identifier, SchemeExp)], env: Env, let: SchemeLetStar, cc: Continuation) extends InternalContinuation

    case class LtrC(i: Identifier, bnd: List[(Identifier, SchemeExp)], env: Env, let: SchemeLetrec, cc: Continuation) extends InternalContinuation

    case class PaiC(car: Value, e: SchemeExp, cc: Continuation) extends InternalContinuation

    case class RetC(addr: Addr, cc: Continuation) extends InternalContinuation // Allows to write the return value of a function call to the store. This breaks tail recursion...
    case class SetC(addr: Addr, cc: Continuation) extends InternalContinuation

    case class TrdC(tid: Int = -1) extends Continuation // End of a thread.

    /* ****************** */
    /* ***** STATES ***** */
    /* ****************** */

    sealed trait State:
        val cc: Continuation

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
      ): State = exp match
        case SchemeAssert(_, _)                            => Kont(Value.Void, cc) // Currently ignored.
        case SchemeBegin(Nil, _)                           => Kont(Value.Void, cc) // Allow empty begin (same than other interpreter).
        case SchemeBegin(first :: Nil, _)                  => Step(first, env, cc)
        case SchemeBegin(first :: rest, _)                 => Step(first, env, BegC(rest, env, cc))
        case SchemeCodeChange(old, _, _) if version == New => Step(old, env, cc)
        case SchemeCodeChange(_, nw, _) if version == Old  => Step(nw, env, cc)
        case SchemeDefineVariable(_, _, _)                 => signalException("Undefined expression expected.")
        case call @ SchemeFuncall(f, args, _)              => Step(f, env, FunC(args, env, call, cc))
        case SchemeIf(cond, cons, alt, _)                  => Step(cond, env, IffC(cons, alt, env, cc))
        case lambda: SchemeLambdaExp                       => Kont(Value.Clo(lambda, env), cc)
        case SchemeLet(Nil, body, pos)                     => Step(SchemeBegin(body, pos), env, cc)
        case let @ SchemeLet((_, e) :: rest, _, _)         => Step(e, env, LetC(rest, env, Nil, let, cc))
        case SchemeLetrec(Nil, body, pos)                  => Step(SchemeBegin(body, pos), env, cc)
        case let @ SchemeLetrec(bnd @ (i, e) :: rest, _, _) =>
          val extEnv = env ++ bnd.map((idf, _) => (idf.name, newAddr(AddrInfo.VarAddr(idf))))
          Step(e, extEnv, LtrC(i, rest, extEnv, let, cc))
        case SchemeLetStar(Nil, body, pos)             => Step(SchemeBegin(body, pos), env, cc)
        case let @ SchemeLetStar((i, e) :: rest, _, _) => Step(e, env, LtsC(i, rest, env, let, cc))
        case SchemeSet(variable, value, _) =>
          env.get(variable.name) match
              case Some(addr) => Step(value, env, SetC(addr, cc))
              case None       => signalException(s"Undefined variable $variable at position ${variable.idn}.")
        case SchemeSetLex(_, _, _, _) => signalException("Unsupported: lexical addresses.")
        case SchemeValue(value, _)    => Kont(evalLiteral(value, exp), cc)
        case SchemeVar(id) =>
          env.get(id.name) match
              case None => signalException(s"Undefined variable $id at position ${id.idn}")
              case Some(addr) =>
                lookupStoreOption(addr) match
                    case None        => signalException(s"Uninitialised variable $id at position ${id.idn}.")
                    case Some(value) => Kont(value, cc)
        case SchemeVarLex(_, _) => signalException("Unsupported: lexical addresses.")

        case CSchemeFork(body, _) =>
          val tid = allocTID()
          work = work.enqueue(Step(body, env, TrdC(tid)))
          Kont(Value.CThread(tid), cc)
        case CSchemeJoin(tExp, _) => Step(tExp, env, JoiC(cc))

        case _ => signalException(s"Unsupported expression type: ${exp.label}.")

    def apply(v: Value, cc: Continuation): State = cc match
        case ArgC(Nil, _, f, argv, call, cc)                 => continueBody(f, v :: argv, call, cc)
        case ArgC(first :: rest, env, f, argv, call, cc)     => Step(first, env, ArgC(rest, env, f, v :: argv, call, cc))
        case BegC(first :: Nil, env, cc)                     => Step(first, env, cc)
        case BegC(first :: rest, env, cc)                    => Step(first, env, BegC(rest, env, cc))
        case FunC(args, env, call, cc)                       => continueArgs(v, args, env, call, cc)
        case IffC(_, alt, env, cc) if v == Value.Bool(false) => Step(alt, env, cc)
        case IffC(cons, _, env, cc)                          => Step(cons, env, cc)
        case JoiC(cc) =>
          v match
              case Value.CThread(tid) =>
                waiting(tid) match
                    case Left(continuations) =>
                      waiting = waiting + (tid -> Left(continuations + cc))
                      scheduleNext()
                      state
                    case Right(v) => Kont(v, cc)
              case v => signalException(s"Cannot join non-thread value: $v.")
        case LetC(Nil, env, bvals, let, cc) => Step(SchemeBegin(let.body, let.idn), extendEnv(let.bindings.map(_._1), (v :: bvals).reverse, env), cc)
        case LetC((_, e) :: rest, env, bvals, let, cc) => Step(e, env, LetC(rest, env, v :: bvals, let, cc))
        case LtrC(i1, bnd, env, let, cc) =>
          extendStore(env(i1.name), v) // Overwrite the previous binding.
          bnd match
              case Nil             => Step(SchemeBegin(let.body, let.idn), env, cc)
              case (i2, e) :: rest => Step(e, env, LtrC(i2, rest, env, let, cc))
        case LtsC(i, Nil, env, let, cc)              => Step(SchemeBegin(let.body, let.idn), extendEnv(env, (i, v)), cc)
        case LtsC(i1, (i2, e) :: rest, env, let, cc) => val extEnv = extendEnv(env, (i1, v)); Step(e, extEnv, LtsC(i2, rest, extEnv, let, cc))
        case PaiC(car, e, cc)                        => Kont(allocateCons(e, car, v), cc)
        case RetC(addr, cc)                          => extendStore(addr, v); Kont(v, cc)
        case SetC(addr, cc)                          => extendStore(addr, v); Kont(Value.Void, cc)

    // Evaluate the arguments of a function.
    def continueArgs(
        f: Value,
        args: List[SchemeExp],
        env: Env,
        call: SchemeFuncall,
        cc: Continuation
      ): State =
        // First, check the arity.
        f match
            case Value.Clo(lambda @ SchemeLambda(_, argNames, _, _, _), _) =>
              if args.length != argNames.length then
                  signalException(
                    s"Invalid function call at position ${call.idn}: ${args.length} arguments given to function ${lambda.lambdaName}, while exactly ${argNames.length} are expected."
                  )
            case Value.Clo(lambda @ SchemeVarArgLambda(_, argNames, _, _, _, _), _) =>
              if args.length < argNames.length then
                  signalException(
                    s"Invalid function call at position ${call.idn}: ${args.length} arguments given to function ${lambda.lambdaName}, while at least ${argNames.length} are expected."
                  )
            case Value.Primitive(_) => // Arity is checked upon primitive call.
            case v                  => signalException(s"Invalid function call at position ${call.idn}: ${v} is not a closure or a primitive.")
        // If the arity is ok, evaluate the arguments.
        args match
            case Nil           => continueBody(f, Nil, call, cc)
            case first :: rest => Step(first, env, ArgC(rest, env, f, Nil, call, cc))

    // Evaluate the body of a function.
    def continueBody(
        f: Value,
        argvsRev: List[Value],
        call: SchemeFuncall,
        cc: Continuation
      ): State =
        val argvs = argvsRev.reverse
        f match
            case Value.Clo(SchemeLambda(_, argNames, body, _, _), env2) =>
              Step(SchemeBody(body), extendEnv(argNames, argvs, env2), cc)
            case Value.Clo(SchemeVarArgLambda(_, argNames, vararg, body, _, _), env2) =>
              val varArgAddr = newAddr(AddrInfo.VarAddr(vararg))
              extendStore(varArgAddr, makeList(call.args.drop(argNames.length).zip(argvs.drop(argNames.length))))
              val envExt = extendEnv(argNames, argvs, env2) + (vararg.name -> varArgAddr)
              Step(SchemeBody(body), envExt, cc)
            case Value.Primitive(p) => Kont(Primitives.allPrimitives(p).call(call, call.args.zip(argvs)), cc)
            case _                  => throw new Exception("Expected a function or primitive to apply")

    def extendEnv(
        ids: List[Identifier],
        values: List[Value],
        env: Env
      ): Env = ids.zip(values).foldLeft(env)(extendEnv)

    def extendEnv(env: Env, idv: (Identifier, Value)): Env =
        val addr = newAddr(AddrInfo.VarAddr(idv._1))
        extendStore(addr, idv._2)
        env + (idv._1.name -> addr)
