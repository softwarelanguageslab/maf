package maf.language.racket

import maf.language.scheme.*

enum ProvideDirective:
    case AllDefinedOut
    case IdentifierOut(name: String)
    case Except(dir: ProvideDirective, excludeList: List[String])

object ProvideDirective:
    def fromExp(exp: SchemeExp): ProvideDirective = ???
