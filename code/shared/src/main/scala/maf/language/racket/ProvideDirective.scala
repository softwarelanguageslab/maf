package maf.language.racket

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
    def fromExp(exp: SchemeExp): ProvideDirective = ???
