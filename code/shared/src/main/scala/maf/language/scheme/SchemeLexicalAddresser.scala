package maf.language.scheme

import maf.core.Identifier

/** Lexical references can either be: */
enum LexicalRef:
    case PrmRef(nam: String)
    case VarRef(id: Identifier)

trait BaseSchemeLexicalAddresser:

    /** A frame is a mapping from variable names to the lexical definition site */
    type Frame = Map[String, Identifier]
    type Scope = List[Frame]

    val emptyFrame: Frame = Map.empty
    val emptyScope: Scope = List.empty

    def lookup(scope: Scope, name: String): Option[Identifier] = scope match
        case Nil           => None
        case frame :: rest => frame.get(name).orElse(lookup(rest, name))

    def extendScope(scope: Scope, id: Identifier): Scope = scope match
        case frame :: _ if frame.contains(id.name) => throw new Exception(s"Duplicate definition of identifier $id")
        case frame :: rest                         => (frame + (id.name -> id)) :: rest
        case Nil                                   => throw new Exception("Unable to extend an empty frame")

    def newFrame(scope: Scope): Scope = Map.empty :: scope

    case class LexicalEnv(scope: Scope)(globals: String => Boolean):
        def newFrame = LexicalEnv(emptyFrame :: scope)(globals)
        def extend(id: Identifier): LexicalEnv = LexicalEnv(extendScope(scope, id))(globals)
        def extend(ids: Iterable[Identifier]): LexicalEnv = ids.foldLeft(this)((acc, idf) => acc.extend(idf))
        def resolve(nam: String): LexicalRef = lookup(scope, nam) match
            case Some(idf)            => LexicalRef.VarRef(idf)
            case None if globals(nam) => LexicalRef.PrmRef(nam)
            case None                 => throw new Exception(s"Undefined variable $nam")

    def initialLEnv(globals: String => Boolean) = LexicalEnv(emptyScope)(globals)

    def translateProgram(prg: List[SchemeExp], globals: String => Boolean = _ => true): List[SchemeExp] =
      translateBody(prg, initialLEnv(globals).newFrame.extend(SchemeBody.defs(prg)))

    def translate(exp: SchemeExp, lenv: LexicalEnv): SchemeExp = exp match
        case vexp: SchemeValue =>
          vexp
        case SchemeLambda(nam, prs, bdy, ann, pos) =>
          SchemeLambda(nam, prs, translateBody(bdy, lenv.newFrame.extend(prs)), ann, pos)
        case SchemeVarArgLambda(nam, prs, vararg, bdy, ann, pos) =>
          SchemeVarArgLambda(nam, prs, vararg, translateBody(bdy, lenv.newFrame.extend(prs).extend(vararg)), ann, pos)
        case SchemeVar(id) =>
          SchemeVarLex(id, lenv.resolve(id.name))
        case SchemeBegin(eps, pos) =>
          SchemeBegin(translate(eps, lenv), pos)
        case SchemeDefineVariable(id, vexp, pos) =>
          SchemeDefineVariable(id, translate(vexp, lenv), pos)
        case SchemeSet(id, vexp, pos) =>
          SchemeSetLex(id, lenv.resolve(id.name), translate(vexp, lenv), pos)
        case SchemeIf(prd, csq, alt, pos) =>
          SchemeIf(translate(prd, lenv), translate(csq, lenv), translate(alt, lenv), pos)
        case SchemeFuncall(fun, args, pos) =>
          SchemeFuncall(translate(fun, lenv), translate(args, lenv), pos)
        case SchemeAssert(exp, pos) =>
          SchemeAssert(translate(exp, lenv), pos)
        case SchemeLet(bindings, body, pos) =>
          val (vrs, eps) = bindings.unzip
          val bdsLex = vrs.zip(translate(eps, lenv))
          val bdyLex = translateBody(body, lenv.newFrame.extend(vrs))
          SchemeLet(bdsLex, bdyLex, pos)
        case SchemeLetStar(bindings, body, pos) =>
          val (bdsLex, extEnv) = bindings.foldLeft[(List[(Identifier, SchemeExp)], LexicalEnv)]((Nil, lenv.newFrame)) {
            case ((accBds, accEnv), (id, vexp)) => ((id, translate(vexp, accEnv)) :: accBds, accEnv.extend(id))
          }
          val bdyLex = translateBody(body, extEnv)
          SchemeLetStar(bdsLex.reverse, bdyLex, pos)
        case SchemeLetrec(bindings, body, pos) =>
          val (vrs, eps) = bindings.unzip
          val extEnv = lenv.newFrame.extend(vrs)
          val bdsLex = vrs.zip(translate(eps, extEnv))
          val bdyLex = translateBody(body, extEnv)
          SchemeLetrec(bdsLex, bdyLex, pos)
        // TODO: HACK: ContractScheme expression should no be here, but no easy way for MutableVarBoxer
        // to make this parametric
        case ContractSchemeMon(contract, expression, idn) =>
          ContractSchemeMon(translate(contract, lenv), translate(expression, lenv), idn)
        case ContractSchemeFlatContract(contract, idn) =>
          ContractSchemeFlatContract(translate(contract, lenv), idn)
        case ContractSchemeDepContract(domains, rangeMaker, idn) =>
          ContractSchemeDepContract(domains.map(translate(_, lenv)), translate(rangeMaker, lenv), idn)
        case ContractSchemeProvide(outs, idn) =>
          ContractSchemeProvide(translateContractSchemeOut(outs, lenv), idn)
        case ContractSchemeCheck(contract, valueExpression, idn) =>
          ContractSchemeCheck(translate(contract, lenv), translate(valueExpression, lenv), idn)
        case m: MakeStruct => m // no variables inside a makestruct expression so lexical adressing does not need to be recursively called
        case _             => throw new Exception(s"Unsupported Scheme expression: $exp")

    def translateContractSchemeOut(outs: List[ContractSchemeProvideOut], lenv: LexicalEnv): List[ContractSchemeProvideOut] =
      outs.map { case ContractSchemeContractOut(name, contract, idn) =>
        ContractSchemeContractOut(name, translate(contract, lenv), idn)
      }

    def translate(bdy: List[SchemeExp], lenv: LexicalEnv): List[SchemeExp] =
      bdy.map(translate(_, lenv))
    def translateBody(body: List[SchemeExp], lenv: LexicalEnv): List[SchemeExp] =
      translate(body, lenv.newFrame.extend(SchemeBody.defs(body)))

object SchemeLexicalAddresser extends BaseSchemeLexicalAddresser
