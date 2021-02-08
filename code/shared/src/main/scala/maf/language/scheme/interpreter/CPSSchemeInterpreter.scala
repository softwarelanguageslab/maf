package maf.language.scheme.interpreter

import maf.core._
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.util.benchmarks.Timeout

class CPSSchemeInterpreter(
    cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[ConcreteValues.Value]
       with ConcreteSchemePrimitives {

  override def run(
      program: SchemeExp,
      timeout: Timeout.T,
      version: Version
    ): ConcreteValues.Value = ???

  def stackedException[R](msg: String): R = ???

  sealed trait Continuation

  case class RefC(cc: Continuation) extends Continuation

  case class AssC(
      v: Identifier,
      bnd: List[(Identifier, SchemeExp)],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class AsmC(v: Identifier, cc: Continuation) extends Continuation

  sealed trait State

  case class Step(
      exp: SchemeExp,
      env: Env,
      version: Version,
      cc: Continuation)
      extends State

  case class Kont(v: ConcreteValues.Value, cc: Continuation) extends State

  def eval(
      exp: SchemeExp,
      env: Env,
      version: Version,
      cc: Continuation
    ): State = exp match {
    case SchemeAnd(exps, _)                                      => ???
    case SchemeAssert(exp, _)                                    => ???
    case SchemeBegin(exps, _)                                    => ???
    case SchemeCodeChange(old, _, _) if version == New           => Step(old, env, version, cc)
    case SchemeCodeChange(_, nw, _) if version == Old            => Step(nw, env, version, cc)
    case SchemeDefineFunction(name, args, body, _)               => ???
    case SchemeDefineVarArgFunction(name, args, vararg, body, _) => ???
    case SchemeDefineVariable(name, value, _)                    => ???
    case SchemeFuncall(f, args, _)                               => ???
    case SchemeIf(cond, cons, alt, _)                            => ???
    case SchemeLambda(args, body, _)                             => ???
    case SchemeLet(bindings, body, _)                            => ???
    case SchemeLetStar(bindings, body, _)                        => ???
    case SchemeLetrec(bindings, body, _)                         => ???
    case SchemeNamedLet(name, bindings, body, _)                 => ???
    case SchemeOr(exps, _)                                       => ???
    case SchemePair(car, cdr, _)                                 => ???
    case SchemeSet(variable, value, _)                           => ???
    case SchemeSetLex(variable, lexAddr, value, _)               => ???
    case SchemeSplicedPair(splice, cdr, _)                       => ???
    case SchemeValue(value, _)                                   => Kont(evalLiteral(value, exp), cc)
    case SchemeVar(id) =>
      env.get(id.name).flatMap(lookupStoreOption).map(Kont(_, cc)).getOrElse(stackedException(s"Unbound variable $id at position ${id.idn}."))
    case SchemeVarArgLambda(args, vararg, body, _) => ???
    case SchemeVarLex(id, lexAddr)                 => ???

    case CSchemeFork(body, _) => ???
    case CSchemeJoin(tExp, _) => ???

    case _ => throw new Exception(s"Unsupported expression type: ${exp.label}.")
  }

  def apply(v: ConcreteValues.Value, cc: Continuation): State = cc match {
    case RefC(cc)              => ???
    case AssC(v, bnd, env, cc) => ???
    case AsmC(v, cc)           => ???
  }
}
