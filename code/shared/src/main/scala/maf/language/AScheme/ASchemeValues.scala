package maf.language.AScheme

import maf.util.SmartHash
import maf.language.scheme.ASchemeActor
import maf.language.scheme.*
import maf.core.{Address, Environment, Identifier}

object ASchemeValues:
    sealed trait ASchemeValue

    /** An actor identifier */
    trait AID extends SmartHash with ASchemeValue

    /** A simple actor identifier based on a sequential integer */
    case class SimpleActorId(id: Int) extends AID

    /**
     * A running actor.
     *
     * @param name
     *   an optional of the actor
     * @param TID
     *   The associated actor id (usually represented by a component)
     */
    case class Actor(name: Option[String], tid: AID) extends ASchemeValue:
        override def toString: String = s"$name"

    /** Represents a behavior */
    case class Behavior(name: Option[String], prs: List[Identifier], bdy: SchemeExp, lexEnv: Environment[Address]) extends ASchemeValue:
        override def toString: String = s"<behavior: $name>"

    def EmptyBehavior(bdy: SchemeExp): Behavior = Behavior(Some("<empty>"), List(), bdy, Environment.empty)

    case class Message[Value](tag: String, vlus: List[Value]) extends ASchemeValue
