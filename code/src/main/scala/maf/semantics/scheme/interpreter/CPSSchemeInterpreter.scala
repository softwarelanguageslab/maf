package maf.semantics.scheme.interpreter

import maf.domain.*
import maf.semantics.scheme.interpreter.ConcreteValues._
import maf.syntax.*
import maf.syntax.scheme.*
import maf.util.Timeout

import scala.concurrent.TimeoutException

class CPSSchemeInterpreter(
    cb: (Option[Position], Value) => Unit = (_, _) => (), // TODO: incorporate
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[Value]
    with ConcreteSchemePrimitives:

    // Note that this variable makes concurrent uses of `run` not thread safe.
    var state: State = _

    protected def next(st: State): Either[Value, State] = st match
        case Step(exp, env, cc) => Right(eval(exp, env, cc))
        case Kont(v, EndC())    => Left(v)
        case Kont(v, cc)        => Right(apply(v, cc))

    override def run(
        program: SchemeExp[Any],
        timeout: Timeout.T
      ): Value =
        setStore(initialSto)
        state = Step(program, initialEnv, EndC())
        try
            while !timeout.reached do
                state = next(state) match
                    case Left(vlu) => return vlu
                    case Right(st) => st
            throw new TimeoutException()
        catch
            // Use the continuations to print the stack trace.
            case e @ ProgramError(message) =>
                var cc = state.cc
                var msg: String = s"$message\n Callstack:"
                if stack then
                    while
                        msg = msg + s"\n * ${cc.toString}"
                        cc = cc.asInstanceOf[InternalContinuation].cc
                        cc != EndC()
                    do ()
                    msg += "\n **********"
                throw e

    /* ************************* */
    /* ***** CONTINUATIONS ***** */
    /* ************************* */

    trait Continuation

    trait InternalContinuation extends Continuation:
        val cc: Continuation

    case class ArgC(
        exps: List[SchemeExp[Any]],
        env: Env,
        f: Value,
        argv: List[Value],
        call: SchemeFuncall[Any],
        cc: Continuation)
        extends InternalContinuation

    case class BegC(exps: List[SchemeExp[Any]], env: Env, cc: Continuation) extends InternalContinuation

    case class EndC() extends Continuation // End of the program.
    case class FunC(args: List[SchemeExp[Any]], env: Env, call: SchemeFuncall[Any], cc: Continuation) extends InternalContinuation

    case class IffC(cons: SchemeExp[Any], alt: SchemeExp[Any], env: Env, cc: Continuation) extends InternalContinuation

    case class LetC(bnd: List[(Identifier, SchemeExp[Any])], env: Env, bvals: List[Value], let: SchemeLet[Any], cc: Continuation) extends InternalContinuation

    case class LtsC(i: Identifier, bnd: List[(Identifier, SchemeExp[Any])], env: Env, let: SchemeLetStar[Any], cc: Continuation) extends InternalContinuation

    case class LtrC(i: Identifier, bnd: List[(Identifier, SchemeExp[Any])], env: Env, let: SchemeLetrec[Any], cc: Continuation) extends InternalContinuation

    case class PaiC(car: Value, e: SchemeExp[Any], cc: Continuation) extends InternalContinuation

    case class RetC(addr: Addr, cc: Continuation) extends InternalContinuation // Allows to write the return value of a function call to the store. This breaks tail recursion...
    case class SetC(addr: Addr, cc: Continuation) extends InternalContinuation

    /* ****************** */
    /* ***** STATES ***** */
    /* ****************** */

    sealed trait State:
        val cc: Continuation

    case class Step(
        exp: SchemeExp[Any],
        env: Env,
        cc: Continuation)
        extends State

    case class Kont(v: Value, cc: Continuation) extends State

    /* ********************** */
    /* ***** EVAL/APPLY ***** */
    /* ********************** */

    def eval(
        exp: SchemeExp[Any],
        env: Env,
        cc: Continuation
      ): State = exp match
        case SchemeAssert(_, _)                            => Kont(Value.Void, cc) // Currently ignored.
        case SchemeBegin(Nil, _)                           => Kont(Value.Void, cc) // Allow empty begin (same than other interpreter).
        case SchemeBegin(first :: Nil, _)                  => Step(first, env, cc)
        case SchemeBegin(first :: rest, _)                 => Step(first, env, BegC(rest, env, cc))
        case SchemeDefineVariable(_, _, pos)               => signalException(UndefinedExpressionExpected(pos))
        case call @ SchemeFuncall(f, args, _)              => Step(f, env, FunC(args, env, call, cc))
        case SchemeIf(cond, cons, alt, _)                  => Step(cond, env, IffC(cons, alt, env, cc))
        case lambda: SchemeLambdaExp[Any]                       => Kont(Value.Clo(lambda, env), cc)
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
                case None       => signalException(UndefinedVariableError(variable))
        case SchemeSetLex(_, _, _, _) => signalException(UnsupportedExpression(exp))
        case SchemeValue(value, _)    => Kont(evalLiteral(value, exp), cc)
        case SchemeVar(id) =>
            env.get(id.name) match
                case None => signalException(UndefinedVariableError(id))
                case Some(addr) =>
                    lookupStoreOption(addr) match
                        case None        => signalException(UninitialisedVariableError(id))
                        case Some(value) => Kont(value, cc)
        case SchemeVarLex(_, _) => signalException(UnsupportedExpression(exp))

        case _ => signalException(UnsupportedExpression(exp))

    def apply(v: Value, cc: Continuation): State = cc match
        case ArgC(Nil, _, f, argv, call, cc)                 => continueBody(f, v :: argv, call, cc)
        case ArgC(first :: rest, env, f, argv, call, cc)     => Step(first, env, ArgC(rest, env, f, v :: argv, call, cc))
        case BegC(first :: Nil, env, cc)                     => Step(first, env, cc)
        case BegC(first :: rest, env, cc)                    => Step(first, env, BegC(rest, env, cc))
        case FunC(args, env, call, cc)                       => continueArgs(v, args, env, call, cc)
        case IffC(_, alt, env, cc) if v == Value.Bool(false) => Step(alt, env, cc)
        case IffC(cons, _, env, cc)                          => Step(cons, env, cc)
        case LetC(Nil, env, bvals, let, cc) => Step(SchemeBegin(let.body, let.pos), extendEnv(let.bindings.map(_._1), (v :: bvals).reverse, env), cc)
        case LetC((_, e) :: rest, env, bvals, let, cc) => Step(e, env, LetC(rest, env, v :: bvals, let, cc))
        case LtrC(i1, bnd, env, let, cc) =>
            extendStore(env(i1.name), v) // Overwrite the previous binding.
            bnd match
                case Nil             => Step(SchemeBegin(let.body, let.pos), env, cc)
                case (i2, e) :: rest => Step(e, env, LtrC(i2, rest, env, let, cc))
        case LtsC(i, Nil, env, let, cc)              => Step(SchemeBegin(let.body, let.pos), extendEnv(env, (i, v)), cc)
        case LtsC(i1, (i2, e) :: rest, env, let, cc) => val extEnv = extendEnv(env, (i1, v)); Step(e, extEnv, LtsC(i2, rest, extEnv, let, cc))
        case PaiC(car, e, cc)                        => Kont(allocateCons(e, car, v), cc)
        case RetC(addr, cc)                          => extendStore(addr, v); Kont(v, cc)
        case SetC(addr, cc)                          => extendStore(addr, v); Kont(Value.Void, cc)

    // Evaluate the arguments of a function.
    def continueArgs(
        f: Value,
        args: List[SchemeExp[Any]],
        env: Env,
        call: SchemeFuncall[Any],
        cc: Continuation
      ): State =
        // First, check the arity.
        f match
            case Value.Clo(lambda @ SchemeLambda(_, argNames, _, _, pos), _) =>
                if args.length != argNames.length then
                    signalException(
                      ArityError(pos, argNames.length, args.length)
                    )
            case Value.Clo(lambda @ SchemeVarArgLambda(_, argNames, _, _, _, pos), _) =>
                if args.length < argNames.length then
                    signalException(
                      VarArityError(pos, argNames.length, args.length)
                    )
            case Value.Primitive(_) => // Arity is checked upon primitive call.
            case v                  => signalException(ValueNotApplicable(v, call.pos))
        // If the arity is ok, evaluate the arguments.
        args match
            case Nil           => continueBody(f, Nil, call, cc)
            case first :: rest => Step(first, env, ArgC(rest, env, f, Nil, call, cc))

    // Evaluate the body of a function.
    def continueBody(
        f: Value,
        argvsRev: List[Value],
        call: SchemeFuncall[Any],
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

    protected def checkValue(v: Value): Boolean = v match
        case Value.Undefined(_) => false
        case _                  => true

    protected def checkAddr(a: Addr): Boolean = a._2 match
        case _: AddrInfo.VarAddr | _: AddrInfo.PtrAddr => true
        case _                                         => false

    override def extendStore(a: Addr, v: Value): Unit =
        if checkAddr(a) && checkValue(v) then cb(a._2.pos, v)
        super.extendStore(a, v)

    def extendEnv(env: Env, idv: (Identifier, Value)): Env =
        val addr = newAddr(AddrInfo.VarAddr(idv._1))
        extendStore(addr, idv._2)
        env + (idv._1.name -> addr)
