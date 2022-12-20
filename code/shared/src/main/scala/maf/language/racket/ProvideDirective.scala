package maf.language.racket

import maf.core.*
import maf.language.scheme.*

enum ProvideDirective:
    case AllDefinedOut
    case IdentifierOut(name: String)
    case Except(dir: ProvideDirective, excludeList: List[String])

    def select(localDefines: List[String]): List[SelectedProvide] = this match
        case AllDefinedOut =>
            // all local defines are provided
            localDefines.map(name => SelectedProvide(name, name))
        case IdentifierOut(name) =>
            // only the given identifier is provided (if it is defined)
            if localDefines.contains(name) then List(SelectedProvide(name, name))
            else sys.error(s"cannot provide $name, undefined")
        case Except(directive, excludeList) =>
            val selectedNames = directive.select(localDefines)
            val toExcl = excludeList.toSet
            selectedNames.filterNot(sel => toExcl.contains(sel.exposedName))

case class SelectedProvide(originalName: String, exposedName: String)

object ProvideDirective:
    def fromExp(exp: SchemeExp): List[ProvideDirective] = exp match
        case RacketProvide(clauses, _) =>
            clauses.map {
                case SchemeVar(Identifier(nam, _)) => IdentifierOut(nam)
                case SchemeFuncall(SchemeVar(Identifier("all-defined-out", _)), List(), _) =>
                    AllDefinedOut

                case SchemeFuncall(SchemeVar(Identifier("except-out", _)), directive :: ids, idn) =>
                    val exceptIds = ids.collect {
                        case SchemeVar(Identifier(name, _)) => name
                        case _                              => sys.error(s"invalid except-in spec at $idn")
                    }
                    val spec = ProvideDirective.fromExp(directive)
                    assert(spec.size == 1)
                    Except(spec.head, exceptIds)

                case e => sys.error(s"invalid provide spec $e")

            }

        case _ => sys.error(s"Invalid expression $exp, expected 'provide'")
