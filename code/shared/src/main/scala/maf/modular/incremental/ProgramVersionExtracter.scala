package maf.modular.incremental

import maf.language.change.CodeVersion._
import maf.language.scheme._

/** Filters the change expressions out of a program and returns the requested version. */
object ProgramVersionExtracter:

    // TODO: make tailrecursive.
    private def getVersion(e: SchemeExp)(implicit version: Version): SchemeExp = e match
        case SchemeLambda(name, args, body, ann, idn)               => SchemeLambda(name, args, body.map(getVersion), ann, idn)
        case SchemeVarArgLambda(name, args, vararg, body, ann, idn) => SchemeVarArgLambda(name, args, vararg, body.map(getVersion), ann, idn)
        case SchemeFuncall(f, args, idn)                            => SchemeFuncall(getVersion(f), args.map(getVersion), idn)
        case SchemeIf(cond, cons, alt, idn)                         => SchemeIf(getVersion(cond), getVersion(cons), getVersion(alt), idn)
        case SchemeLet(bindings, body, idn)              => SchemeLet(bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
        case SchemeLetStar(bindings, body, idn)          => SchemeLetStar(bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
        case SchemeLetrec(bindings, body, idn)           => SchemeLetrec(bindings.map(b => (b._1, getVersion(b._2))), body.map(getVersion), idn)
        case SchemeSet(variable, value, idn)             => SchemeSet(variable, getVersion(value), idn)
        case SchemeSetLex(variable, lexAddr, value, idn) => SchemeSetLex(variable, lexAddr, getVersion(value), idn)
        case SchemeBegin(exps, idn)                      => SchemeBegin(exps.map(getVersion), idn)
        case SchemeDefineVariable(name, value, idn)      => SchemeDefineVariable(name, getVersion(value), idn)

        // Assume no nested changes.
        case SchemeCodeChange(old, _, _) if version == Old => old
        case SchemeCodeChange(_, nw, _)                    => nw

        case CSchemeJoin(tExp, idn) => CSchemeJoin(getVersion(tExp), idn)
        case CSchemeFork(body, idn) => CSchemeFork(getVersion(body), idn)

        case exp => exp

    def getInitial(program: SchemeExp): SchemeExp = getVersion(program)(Old)
    def getUpdated(program: SchemeExp): SchemeExp = getVersion(program)(New)
