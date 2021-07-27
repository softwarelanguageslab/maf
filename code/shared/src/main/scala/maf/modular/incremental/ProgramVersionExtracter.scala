package maf.modular.incremental

import maf.language.change.CodeVersion._
import maf.language.scheme._

/** Filters the change expressions out of a program and returns the requested version. */
object ProgramVersionExtracter {

  // TODO: make tailrecursive.
  private def getVersion(e: SchemeExp)(implicit version: Version): SchemeExp = e match {
    case SchemeLambda(name, args, body, idn)               => SchemeLambda(name, args, body.map(getVersion), idn)
    case SchemeVarArgLambda(name, args, vararg, body, idn) => SchemeVarArgLambda(name, args, vararg, body.map(getVersion), idn)
    case SchemeFuncall(f, args, idn)                       => SchemeFuncall(getVersion(f), args.map(getVersion), idn)
    case SchemeIf(cond, cons, alt, idn)                    => SchemeIf(getVersion(cond), getVersion(cons), getVersion(alt), idn)
    case SchemeLet(bindings, body, idn)                    => SchemeLet(bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
    case SchemeLetStar(bindings, body, idn)                => SchemeLetStar(bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
    case SchemeLetrec(bindings, body, idn)                 => SchemeLetrec(bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
    case SchemeNamedLet(name, bindings, body, idn)   => SchemeNamedLet(name, bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
    case SchemeSet(variable, value, idn)             => SchemeSet(variable, getVersion(value), idn)
    case SchemeSetLex(variable, lexAddr, value, idn) => SchemeSetLex(variable, lexAddr, getVersion(value), idn)
    case SchemeBegin(exps, idn)                      => SchemeBegin(exps.map(getVersion), idn)
    case SchemeAnd(exps, idn)                        => SchemeAnd(exps.map(getVersion), idn)
    case SchemeOr(exps, idn)                         => SchemeOr(exps.map(getVersion), idn)
    case SchemeDefineVariable(name, value, idn)      => SchemeDefineVariable(name, getVersion(value), idn)
    case SchemeDefineFunction(name, args, body, idn) => SchemeDefineFunction(name, args, body.map(getVersion), idn)
    case SchemeDefineVarArgFunction(name, args, vararg, body, idn) => SchemeDefineVarArgFunction(name, args, vararg, body.map(getVersion), idn)
    case SchemePair(car, cdr, idn)                                 => SchemePair(getVersion(car), getVersion(cdr), idn)
    case SchemeSplicedPair(splice, cdr, idn)                       => SchemeSplicedPair(getVersion(splice), getVersion(cdr), idn)

    // Assume no nested changes.
    case SchemeCodeChange(old, _, _) if version == Old => old
    case SchemeCodeChange(_, nw, _)                    => nw

    case CSchemeJoin(tExp, idn) => CSchemeJoin(getVersion(tExp), idn)
    case CSchemeFork(body, idn) => CSchemeFork(getVersion(body), idn)

    case exp => exp
  }

  def getInitial(program: SchemeExp): SchemeExp = getVersion(program)(Old)
  def getUpdated(program: SchemeExp): SchemeExp = getVersion(program)(New)
}
