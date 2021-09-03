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
object SchemeMutableVarBoxer {
  
    // The transformation works in two phases
    // - first, it extracts all mutable variables from the given program
    // - then, it rewrites:
    //      * all definitions of these variables using `new-ref`
    //      * all assignments to these variables using `set-ref!`
    //      * all references to these variables using `deref`
    def transform(exp: SchemeExp, globals: Set[String]): SchemeExp = {
        // first, collect all mutable vars
        var mutable: Set[LexicalRef] = Set.empty
        object LexicalTranslator extends BaseSchemeLexicalAddresser {
            override def translate(exp: SchemeExp, lenv: LexicalEnv): SchemeExp = super.translate(exp, lenv) match {
                case res@SchemeSetLex(_, ref, _, _) => 
                    mutable += ref
                    res
                case res => res
            }
        }
        val translated = LexicalTranslator.translateProgram(exp, globals)
        // then, rewrite for mutable vars
        rewriteProgram(translated, mutable)
    }

    type Rewrites = Map[LexicalRef, String]

    private def genSym(nam: String) = s"__ref_var_$nam"

    private def rewriteProgram(exp: SchemeExp, mut: Set[LexicalRef]): SchemeExp =
        val mutPrms = mut.collect[LexicalRef.PrmRef] { case prm: LexicalRef.PrmRef => prm }
        val rewPrms = mutPrms.map { prm => (prm, genSym(prm.nam)) }
        if rewPrms.isEmpty then 
            rewrite(exp, mut, Map.empty)
        else
            val bds = rewPrms.toList.map { (prm: LexicalRef.PrmRef, gen: String) => 
                (Identifier(gen, Identity.none), SchemeRef(SchemeVar(Identifier(prm.nam, Identity.none)), Identity.none))
            }
            SchemeLet(bds, List(rewrite(exp, mut, rewPrms.toMap[LexicalRef, String])), Identity.none)

    private def rewrite(exp: SchemeExp, mut: Set[LexicalRef], rew: Rewrites): SchemeExp = exp match {
        case vlu: SchemeValue => vlu
        case SchemeVarLex(id, lex) =>
            if mut(lex) then
                SchemeDeref(SchemeVar(Identifier(rew.getOrElse(lex, id.name), id.idn)), id.idn)
            else
                SchemeVar(id)
        case SchemeSetLex(id, lex, vexp, idn) =>
            assert(mut(lex)) // must mean that lex is marked as mutable!
            SchemeSetRef(SchemeVar(Identifier(rew.getOrElse(lex, id.name), id.idn)),
                         rewrite(vexp, mut, rew),
                         idn)
        case SchemeDefineVariable(id, vexp, pos) =>
            if mut(LexicalRef.VarRef(id)) then 
                SchemeDefineVariable(Identifier(id.name, Identity.none), SchemeRef(rewrite(vexp, mut, rew), id.idn), pos)
            else 
                SchemeDefineVariable(id, rewrite(vexp, mut, rew), pos)
        case SchemeLambda(nam, prs, bdy, pos) =>
            SchemeLambda(nam, prs, rewriteLambdaBody(prs, bdy, mut, rew), pos)
        case SchemeVarArgLambda(nam, prs, vararg, bdy, pos) =>
            SchemeVarArgLambda(nam, prs, vararg, rewriteLambdaBody(prs :+ vararg, bdy, mut, rew), pos)
        case SchemeDefineFunction(id, prs, bdy, pos) =>
            SchemeDefineFunction(id, prs, rewriteLambdaBody(prs, bdy, mut, rew), pos)
        case SchemeDefineVarArgFunction(id, prs, vararg, bdy, pos) =>
            SchemeDefineVarArgFunction(id, prs, vararg, rewriteLambdaBody(prs :+ vararg, bdy, mut, rew), pos)
        case SchemeBegin(eps, pos) =>
            SchemeBegin(eps.map(rewrite(_, mut, rew)), pos)
        case SchemeIf(prd, csq, alt, pos) =>
            SchemeIf(rewrite(prd, mut, rew), rewrite(csq, mut, rew), rewrite(alt, mut, rew), pos)
        case SchemePair(car, cdr, pos) =>
            SchemePair(rewrite(car, mut, rew), rewrite(cdr, mut, rew), pos)
        case SchemeSplicedPair(splice, cdr, pos) =>
            SchemeSplicedPair(rewrite(splice, mut, rew), rewrite(cdr, mut, rew), pos)
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
    }

    private def rewriteBindings(bds: List[(Identifier, SchemeExp)], mut: Set[LexicalRef], rew: Rewrites): List[(Identifier, SchemeExp)] = 
        bds.map { (idf, exp) => 
            if mut(LexicalRef.VarRef(idf)) then 
                (Identifier(idf.name, Identity.none), SchemeRef(rewrite(exp, mut, rew), idf.idn)) 
            else 
                (idf, rewrite(exp, mut, rew))
        }

    private def rewriteLambdaBody(prs: List[Identifier], bdy: List[SchemeExp], mut: Set[LexicalRef], rew: Rewrites): List[SchemeExp] =
        val mutPrs = prs.map(LexicalRef.VarRef(_): LexicalRef.VarRef).filter(mut)
        val rewPrs = mutPrs.map(ref => (ref, genSym(ref.id.name)))
        if rewPrs.isEmpty then 
            rewriteBody(bdy, mut, rew)
        else 
            val bds = rewPrs.map { (ref: LexicalRef.VarRef, gen: String) => 
                (Identifier(gen, Identity.none), SchemeRef(SchemeVar(Identifier(ref.id.name, Identity.none)), ref.id.idn))
            }
            List(SchemeLet(bds, rewriteBody(bdy, mut, rew ++ rewPrs), Identity.none))

    private def rewriteBody(bdy: List[SchemeExp], mut: Set[LexicalRef], rew: Rewrites): List[SchemeExp] =
        bdy.map(rewrite(_, mut, rew))
}
