package maf.language.racket

import maf.language.scheme.*
import maf.core.*

enum RequireDirective:
    case RequireFile(name: String)
    case Prefix(directive: RequireDirective, prefix: String)
    case Except(directive: RequireDirective, excludeList: List[String])
    case Rename(directive: RequireDirective, rename: Map[String, String])

    /** Behidn each directive there is one module/file name */
    def moduleName: Modules.Name = this match
        case RequireFile(name) => Modules.name(name)
        case Prefix(dir, _)    => dir.moduleName
        case Except(dir, _)    => dir.moduleName
        case Rename(dir, _)    => dir.moduleName

    def resolve(resolvedModules: List[RacketModule]): List[ResolvedRequire] = this match
        case RequireFile(name) =>
            // require simply requires all the identifiers in the given module
            val mod = resolvedModules.find(_.name == Modules.name(name)).getOrElse(sys.error(s"Module with name $name not found"))
            mod.provides.map(_.exposedName).map(id => ResolvedRequire(name, id, id))
        case Prefix(directive, prefix) =>
            // a prefix directive prefixes the exposedName with the given prefix
            val resolvedRequires = directive.resolve(resolvedModules)
            resolvedRequires.map(include => include.copy(exposedName = s"$prefix${include.exposedName}"))
        case Except(directive, excludeList) =>
            // except filters elements from the required file
            val resolvedRequires = directive.resolve(resolvedModules)
            resolvedRequires.filterNot(incl => excludeList.toSet.contains(incl.exposedName))
        case Rename(directive, mapping) =>
            // renames the exposed name for the given identifiers
            val resolvedRequires = directive.resolve(resolvedModules)
            resolvedRequires.map(incl => incl.copy(exposedName = mapping.get(incl.exposedName).getOrElse(incl.exposedName)))

case class ResolvedRequire(module: String, originalName: String, exposedName: String):
    def toLoad: SchemeExp = RacketModuleLoad(SchemeVar(Identifier(module, Identity.none)), Identifier(originalName, Identity.none), Identity.none)
    def toDefine: SchemeExp = SchemeDefineVariable(Identifier(exposedName, Identity.none), toLoad, Identity.none)

object RequireDirective:
    def fromExp(e: SchemeExp): RequireDirective = ???
