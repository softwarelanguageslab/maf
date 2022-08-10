package maf.language.racket

enum ProvideDirective:
    case AllDefinedOut
    case IdentifierOut(name: String)
    case Except(dir: ProvideDirective, excludeList: List[String])
