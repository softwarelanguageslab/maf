package maf.language.AScheme

import maf.language.scheme.BaseSchemeLexicalAddresser
import maf.language.scheme.{ASchemeActor, ASchemeBecome, ASchemeCreate, ASchemeSelect, ASchemeSend, SchemeExp}

object ASchemeLexicalAddresser extends BaseSchemeLexicalAddresser:
    override def translate(exp: SchemeExp, lenv: LexicalEnv) = exp match
        case ASchemeSend(ref, msgTpy, args, idn) =>
            ASchemeSend(translate(ref, lenv), msgTpy, args.map(translate(_, lenv)), idn)
        case ASchemeCreate(beh, arguments, idn) =>
            ASchemeCreate(translate(beh, lenv), arguments.map(translate(_, lenv)), idn)
        case ASchemeBecome(beh, args, idn) =>
            ASchemeBecome(translate(beh, lenv), args.map(translate(_, lenv)), idn)
        case ASchemeActor(prs, ASchemeSelect(hndlrs, selIdn), idn, name, isMirror) =>
            val bdyLenv = lenv.newFrame.extend(prs)
            val lexHandlers = hndlrs.map { case (msg, (prs, bdy)) =>
                val hndlLenv = bdyLenv.newFrame.extend(prs)
                (msg -> (prs, translate(bdy, hndlLenv)))
            }
            ASchemeActor(prs, ASchemeSelect(lexHandlers.toMap, selIdn), idn, name, isMirror)
        case _ => super.translate(exp, lenv)
