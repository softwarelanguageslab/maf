package maf.language.AScheme

import maf.util.SmartHash
import maf.language.scheme.ASchemeActor
import maf.language.scheme.*
import maf.core.{Address, Environment, Identifier}

object ASchemeValues:
    /** An actor identifier */
    trait AID extends SmartHash

    /**
     * A running actor.
     *
     * @param name
     *   an optional of the actor
     * @param TID
     *   The associated actor id (usually represented by a component)
     */
    case class Actor(name: Option[String], tid: AID)

    /** Represents a behavior */
    case class Behavior(name: Option[String], prs: List[Identifier], bdy: SchemeExp):
        override def toString: String = s"<behavior: $name>"

    def EmptyBehavior(bdy: SchemeExp): Behavior = Behavior(Some("<empty>"), List(), bdy)
