package maf.language.AScheme

import maf.language.scheme.BaseSchemeCompiler
import maf.language.sexp.*
import maf.language.scheme.*
import maf.core.Identifier
import scala.util.control.TailCalls
import maf.language.scheme.SchemeExpander.{ExpansionContext, SchemeExpanderInterpolator}

object ASchemeCompiler extends BaseSchemeCompiler:
    import scala.util.control.TailCalls._
    import maf.language.sexp.SExpUtils._

    private val actorReserved = List("actor", "send", "become", "create")
    override def reserved: List[String] = actorReserved ::: super.reserved

    private def compileHandler(sexp: SExp): TailRec[(String, (List[Identifier], List[SchemeExp]))] = sexp match
        case IdentWithIdentity(msgTpy, idn) :::: prs :::: bdy =>
            for
                cplArgsComp <- compileArgs(prs)
                (cplArgs, None) = cplArgsComp
                cplBdy <- compileBody(bdy)
            yield (msgTpy -> (cplArgs, cplBdy))
        case _ => throw new Exception(s"Invalid syntax for actor message handler at ${sexp.idn}")

    private def compileHandlers(sexp: SExp): TailRec[List[(String, (List[Identifier], List[SchemeExp]))]] = sexp match
        case SNil(_) => done(List())
        case handler :::: handlers =>
            for
                cplHandler <- tailcall(compileHandler(handler))
                cplHandlers <- tailcall(compileHandlers(handlers))
            yield cplHandler :: cplHandlers

        case _ => throw new Exception(s"Invalid syntax for actor message handler list at ${sexp.idn}")

    override def _compile(exp: SExp): TailCalls.TailRec[SchemeExp] = exp match
        case IdentWithIdentity("actor", actorIdn) :::: SExpValue(Value.String(name), _) :::: args :::: msgs =>
            for
                cplArgsComp <- tailcall(compileArgs(args))
                (cplArgs, None) = cplArgsComp
                cplMsgs <- tailcall(compileHandlers(msgs))
            yield ASchemeActor(cplArgs, ASchemeSelect(cplMsgs.toMap, args.idn), actorIdn, Some(name))

        case IdentWithIdentity("actor", actorIdn) :::: args :::: msgs =>
            for
                cplArgsComp <- tailcall(compileArgs(args))
                (cplArgs, None) = cplArgsComp
                cplMsgs <- tailcall(compileHandlers(msgs))
            yield ASchemeActor(cplArgs, ASchemeSelect(cplMsgs.toMap, args.idn), actorIdn, None)

        case IdentWithIdentity("become", becomeIdn) :::: beh :::: args =>
            for
                cplBeh <- tailcall(_compile(beh))
                cplArgs <- tailcall(compileBody(args))
            yield ASchemeBecome(cplBeh, cplArgs, becomeIdn)

        case IdentWithIdentity("send", sendIdn) :::: actorRef :::: IdentWithIdentity(mTpy, idn) :::: args =>
            for
                cplActorRef <- tailcall(_compile(actorRef))
                cplArgs <- tailcall(compileBody(args))
            yield ASchemeSend(cplActorRef, Identifier(mTpy, idn), cplArgs, sendIdn)

        case IdentWithIdentity("create", createIdn) :::: beh :::: args =>
            for
                cplBeh <- tailcall(_compile(beh))
                cplArgs <- tailcall(compileBody(args))
            yield ASchemeCreate(cplBeh, cplArgs, createIdn)

        // (ask actorRef tag args)
        case Ident("ask") :::: actorRef :::: tag :::: args =>
            given ExpansionContext = ExpansionContext(exp.idn)
            tailcall(_compile(expand"""
            (let ((a (create (actor () (answer (v) (terminate v))))))
              (send $actorRef $tag a . $args)
              (wait-for-termination a))
            """))

        case Ident("reply") :::: actorRef :::: args =>
            given ExpansionContext = ExpansionContext(exp.idn)
            tailcall(_compile(expand"""
              (send $actorRef answer . $args)"""))

        case Ident("await") :::: future :::: SNil(_) =>
            tailcall(_compile(future)).map(ASchemeAwait(_, exp.idn))

        case Ident(tag) if actorReserved.contains(tag) =>
            throw new Exception(s"Invalid syntax for $tag at ${exp.idn}")
        case _ => super._compile(exp)
