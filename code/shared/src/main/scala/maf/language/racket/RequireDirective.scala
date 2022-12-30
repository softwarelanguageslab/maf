package maf.language.racket

import maf.language.scheme.*
import maf.core.*
import maf.language.sexp.Value

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
    private def parsePairs(ps: List[SchemeExp]): Map[String, String] = ps.map {
        case SchemeFuncall(SchemeVar(Identifier(from, _)), List(SchemeVar(Identifier(to, _))), idn) =>
            (from -> to)
        case e => sys.error(s"invalid syntax at ${e.idn}")
    }.toMap

    def fromExp(e: SchemeExp): List[RequireDirective] = e match
        case RacketRequire(clauses, _) =>
            clauses.map {
                case SchemeVar(Identifier(id, _))      => RequireFile(id)
                case SchemeValue(Value.String(s), idn) => RequireFile(s)
                case SchemeFuncall(SchemeVar(Identifier("prefix-in", _)), List(SchemeVar(Identifier(prefixId, _)), directive), idn) =>
                    val spec = RequireDirective.fromExp(directive)
                    assert(spec.size == 1)
                    Prefix(spec.head, prefixId)

                case SchemeFuncall(SchemeVar(Identifier("except-in", _)), directive :: ids, idn) =>
                    val exceptIds = ids.collect {
                        case SchemeVar(Identifier(name, _)) => name
                        case _                              => sys.error(s"invalid except-in spec at $idn")
                    }
                    val spec = RequireDirective.fromExp(directive)
                    assert(spec.size == 1)
                    Except(spec.head, exceptIds)

                case SchemeFuncall(SchemeVar(Identifier("rename-in", _)), directive :: pairs, idn) =>
                    val spec = RequireDirective.fromExp(directive)
                    assert(spec.size == 1)
                    Rename(spec.head, parsePairs(pairs))

                case _ => sys.error(s"invalid require directive $e")

            }
        case _ => sys.error(s"Invalid expression $e, expected 'require'")
