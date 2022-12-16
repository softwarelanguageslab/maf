package maf.language.racket

import maf.language.scheme.SchemeExp

enum RequireDirective:
    case RequireFile(name: String)
    case Prefix(directive: RequireDirective, prefix: String)
    case Except(directive: RequireDirective, exludeList: List[String])
    case Rename(directive: RequireDirective, rename: Map[String, String])

    /** Behidn each directive there is one module/file name */
    def moduleName: String = this match
        case RequireFile(name) => name
        case Prefix(dir, _)    => dir.moduleName
        case Except(dir, _)    => dir.moduleName
        case Rename(dir, _)    => dir.moduleName

object RequireDirective:
    def fromExp(e: SchemeExp): RequireDirective = ???
