package maf.language.scheme

import maf.core._
import maf.language.sexp._
import scala.util.parsing.combinator.lexical.Lexical

// replaces all mutable Scheme variables with immutable variables that use explicitly mutable reference values
// e.g., a program such as
//  (let ((x 1)
//        (y 2))
//   (set! y 10)
//   (+ x y))
// would be rewritten as
//  (let ((x 1)
//        (y (ref 2)))
//    (set-ref! y 10)
//    (+ x (deref y)))
// The main advantage of this transformation is that all variables themselves therefore become immutable.
trait BaseSchemeMutableVarBoxer:
    object LexicalTranslator extends BaseSchemeLexicalAddresser:
        var mutable: Set[LexicalRef] = Set.empty

        override def translate(exp: SchemeExp, lenv: LexicalEnv): SchemeExp = super.translate(exp, lenv) match
            case res @ SchemeSetLex(_, ref, _, _) =>
              mutable += ref
              res
            case res => res

    def lexicalTranslator: LexicalTranslator.type = LexicalTranslator

    // The transformation works in two phases
    // - first, it extracts all mutable variables from the given program
    // - then, it rewrites:
    //      * all definitions of these variables using `new-ref`
    //      * all assignments to these variables using `set-ref!`
    //      * all references to these variables using `deref`
    def transform(exp: List[SchemeExp]): List[SchemeExp] =
        // first, collect all mutable vars
        val translator = lexicalTranslator
        val translated = translator.translateProgram(exp)
        // then, rewrite for mutable vars
        rewriteProgram(translated, translator.mutable)

    type Rewrites = Map[LexicalRef, Identifier]

    private def genSym(nam: String) = s"__ref_var_$nam"

    private def rewriteProgram(exp: List[SchemeExp], mut: Set[LexicalRef]): List[SchemeExp] =
        val mutPrms = mut.collect[LexicalRef.PrmRef] { case prm: LexicalRef.PrmRef => prm }
        val rewPrms = mutPrms.map { prm => (prm, Identifier(genSym(prm.nam), Identity.none)) }
        if rewPrms.isEmpty then rewriteBody(exp, mut, Map.empty)
        else
            val bds = rewPrms.toList.map { (prm: LexicalRef.PrmRef, idf: Identifier) =>
              (idf, SchemeRef(SchemeVar(Identifier(prm.nam, Identity.none)), Identity.none))
            }
            List(SchemeLet(bds, rewriteBody(exp, mut, rewPrms.toMap[LexicalRef, Identifier]), Identity.none))

    private def varRef(rew: Rewrites, id: Identifier, lex: LexicalRef): SchemeVar =
      rew.get(lex) match
          case Some(oth) => SchemeVar(Identifier(oth.name, id.idn)) // SchemeVarLex(Identifier(oth.name, id.idn), LexicalRef.VarRef(oth))
          case None      => SchemeVar(id) // SchemeVarLex(id, lex)

    protected def rewrite(exp: SchemeExp, mut: Set[LexicalRef], rew: Rewrites): SchemeExp = exp match
        case vlu: SchemeValue => vlu
        case SchemeVarLex(id, lex) =>
          if mut(lex) then SchemeDeref(varRef(rew, id, lex), id.idn)
          else SchemeVar(id)
        case SchemeSetLex(id, lex, vexp, idn) =>
          assert(mut(lex)) // must mean that lex is marked as mutable!
          SchemeSetRef(varRef(rew, id, lex), rewrite(vexp, mut, rew), idn)
        case SchemeDefineVariable(id, vexp, pos) =>
          if mut(LexicalRef.VarRef(id)) then SchemeDefineVariable(Identifier(id.name, Identity.none), SchemeRef(rewrite(vexp, mut, rew), id.idn), pos)
          else SchemeDefineVariable(id, rewrite(vexp, mut, rew), pos)
        case SchemeLambda(nam, prs, bdy, ann, pos) =>
          SchemeLambda(nam, prs, rewriteLambdaBody(prs, bdy, mut, rew), ann, pos)
        case SchemeVarArgLambda(nam, prs, vararg, bdy, ann, pos) =>
          SchemeVarArgLambda(nam, prs, vararg, rewriteLambdaBody(prs :+ vararg, bdy, mut, rew), ann, pos)
        case SchemeBegin(eps, pos) =>
          SchemeBegin(eps.map(rewrite(_, mut, rew)), pos)
        case SchemeIf(prd, csq, alt, pos) =>
          SchemeIf(rewrite(prd, mut, rew), rewrite(csq, mut, rew), rewrite(alt, mut, rew), pos)
        case SchemeFuncall(fun, args, pos) =>
          SchemeFuncall(rewrite(fun, mut, rew), args.map(rewrite(_, mut, rew)), pos)
        case SchemeAssert(exp, pos) =>
          SchemeAssert(rewrite(exp, mut, rew), pos)
        case SchemeLet(bds, body, pos) =>
          SchemeLet(rewriteBindings(bds, mut, rew), rewriteBody(body, mut, rew), pos)
        case SchemeLetStar(bds, body, pos) =>
          SchemeLetStar(rewriteBindings(bds, mut, rew), rewriteBody(body, mut, rew), pos)
        case SchemeLetrec(bds, body, pos) =>
          SchemeLetrec(rewriteBindings(bds, mut, rew), rewriteBody(body, mut, rew), pos)

    private def rewriteBindings(bds: List[(Identifier, SchemeExp)], mut: Set[LexicalRef], rew: Rewrites): List[(Identifier, SchemeExp)] =
      bds.map { (idf, exp) =>
        if mut(LexicalRef.VarRef(idf)) then (Identifier(idf.name, Identity.none), SchemeRef(rewrite(exp, mut, rew), idf.idn))
        else (idf, rewrite(exp, mut, rew))
      }

    private def rewriteLambdaBody(prs: List[Identifier], bdy: List[SchemeExp], mut: Set[LexicalRef], rew: Rewrites): List[SchemeExp] =
        val mutPrs = prs.map(LexicalRef.VarRef(_): LexicalRef.VarRef).filter(mut)
        val rewPrs = mutPrs.map(ref => (ref, Identifier(genSym(ref.id.name), Identity.none)))
        if rewPrs.isEmpty then rewriteBody(bdy, mut, rew)
        else
            val bds = rewPrs.map { (ref: LexicalRef.VarRef, idf: Identifier) =>
              (idf, SchemeRef(SchemeVar(Identifier(ref.id.name, Identity.none)), ref.id.idn))
            }
            List(SchemeLet(bds, rewriteBody(bdy, mut, rew ++ rewPrs), Identity.none))

    private def rewriteBody(bdy: List[SchemeExp], mut: Set[LexicalRef], rew: Rewrites): List[SchemeExp] =
      bdy.map(rewrite(_, mut, rew))

object SchemeMutableVarBoxer extends BaseSchemeMutableVarBoxer

//
// Extra utility to extract all top-level vars of a program
// (these can sometimes be treated differently in the analysis)
//

object SchemeTopLevelVars:
    def collect(exp: SchemeExp): Set[Identifier] = exp match
        case _: SchemeValue | _: SchemeVar | _: SchemeLambda | _: SchemeVarArgLambda => Set.empty
        case SchemeSet(_, vexp, _)                                                   => collect(vexp)
        case SchemeDefineVariable(_, vexp, _)                                        => collect(vexp)
        case SchemeBegin(eps, _)                                                     => collect(eps)
        case SchemeIf(prd, csq, alt, _)                                              => collect(prd) ++ collect(csq) ++ collect(alt)
        case SchemeFuncall(fun, ags, _)                                              => collect(fun) ++ collect(ags)
        case SchemeAssert(cnd, _)                                                    => collect(cnd)
        case let: SchemeLettishExp =>
          val (ids, eps) = let.bindings.unzip
          ids.toSet ++ collect(eps) ++ collect(let.body)
        case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
    def collect(eps: List[SchemeExp]): Set[Identifier] =
      eps.foldLeft(Set.empty)((acc, exp) => acc ++ collect(exp))
