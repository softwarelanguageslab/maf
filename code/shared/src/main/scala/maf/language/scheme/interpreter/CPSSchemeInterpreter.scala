package maf.language.scheme.interpreter

import maf.core._
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.language.sexp.SExp

class CPSSchemeInterpreter(
    cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
    val io: IO = new EmptyIO(),
    val stack: Boolean = false)
    extends BaseSchemeInterpreter[ConcreteValues.Value]
       with ConcreteSchemePrimitives {

  def newAddr(meta: ConcreteValues.AddrInfo): (Int, ConcreteValues.AddrInfo) = ???

  var store: Map[(Int, ConcreteValues.AddrInfo), ConcreteValues.Value] = Map()

  def extendStore(a: (Int, ConcreteValues.AddrInfo), v: ConcreteValues.Value): Unit = ???

  def lookupStore(a: (Int, ConcreteValues.AddrInfo)): ConcreteValues.Value = ???

  def lookupStoreOption(a: (Int, ConcreteValues.AddrInfo)): Option[ConcreteValues.Value] = ???

  def setStore(s: Map[(Int, ConcreteValues.AddrInfo), ConcreteValues.Value]): Unit = ???

  def allocateVal(exp: SchemeExp, value: ConcreteValues.Value): Value.Pointer = ???

  def allocateCons(
      exp: SchemeExp,
      car: ConcreteValues.Value,
      cdr: ConcreteValues.Value
    ): ConcreteValues.Value = ???

  def allocateStr(exp: SchemeExp, str: String): Value.Pointer = ???

  def getString(addr: (Int, ConcreteValues.AddrInfo)): String = ???

  def makeList(values: List[(SchemeExp, ConcreteValues.Value)]): ConcreteValues.Value = ???

  def stackedCall(
      name: Option[String],
      idn: Identity,
      block: => ConcreteValues.Value
    ): ConcreteValues.Value = ???

  def stackedException[R](msg: String): R = ???

  def evalSExp(sexp: SExp, exp: SchemeExp): ConcreteValues.Value = ???

  sealed trait Continuation

  case class RefC(cc: Continuation) extends Continuation

  case class AssC(
      v: Identifier,
      bnd: List[(Identifier, SchemeExp)],
      env: Env,
      cc: Continuation)
      extends Continuation

  case class AsmC(v: Identifier, cc: Continuation) extends Continuation

  def eval(
      exp: SchemeExp,
      env: Env,
      version: Version,
      cc: Continuation
    ): ConcreteValues.Value = exp match { // TODO fix return type
    case SchemeAnd(exps, _)                                      => ???
    case SchemeAssert(exp, _)                                    => ???
    case SchemeBegin(exps, _)                                    => ???
    case SchemeCodeChange(old, _, _) if version == New           => eval(old, env, version, cc)
    case SchemeCodeChange(_, nw, _) if version == Old            => eval(nw, env, version, cc)
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
    case SchemeValue(value, _)                                   => ???
    case SchemeVar(id)                                           => ???
    case SchemeVarArgLambda(args, vararg, body, _)               => ???
    case SchemeVarLex(id, lexAddr)                               => ???

    case CSchemeFork(body, _) => ???
    case CSchemeJoin(tExp, _) => ???

    case _ => throw new Exception(s"Unsupported expression type: ${exp.label}.")
  }

  def apply(v: ConcreteValues.Value, cc: Continuation): ConcreteValues.Value = cc match {
    case RefC(cc)              => ???
    case AssC(v, bnd, env, cc) => ???
    case AsmC(v, cc)           => ???
  }
}
