package maf.language.AScheme.interpreter

import maf.language.AScheme.ASchemeValues.*
import maf.language.scheme.interpreter.ConcreteValues.*
import maf.core.Identity

object ConcreteASchemeValues:
    case class ConcreteActorValue(actorValue: ASchemeValue) extends Value
    case class ConcreteActor(name: Option[String], tid: AID, initialBeh: Behavior, creationSite: Identity) extends Value
    implicit def toConcreteActorValue(actorValue: ASchemeValue): Value = ConcreteActorValue(actorValue)
