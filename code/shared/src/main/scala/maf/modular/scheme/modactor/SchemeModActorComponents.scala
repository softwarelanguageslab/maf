package maf.modular.scheme.modactor

import maf.util.*
import maf.language.CScheme.*
import maf.language.scheme.*
import maf.core.*
import maf.language.AScheme.ASchemeValues.AID
import maf.language.AScheme.ASchemeValues.Behavior
import maf.modular.scheme.modf.SchemeModFComponent

sealed trait SchemeModActorComponent[+Context] extends SmartHash with AID:
    def removeContext: SchemeModActorComponent[Unit]
    override def removeEnv: SchemeModActorComponent[Context]

// The main actor of the program
case object MainActor extends SchemeModActorComponent[Nothing]:
    def removeContext: SchemeModActorComponent[Unit] = MainActor
    def removeEnv: SchemeModActorComponent[Nothing] = MainActor

/*
 * An actor that was spawned in some other actor.
 *
 * @param beh the initial behaviour of the actor
 * @param env the initial environment of the actor
 * @param ctx an optional context, to be used to change the sensitivity of the analysis
 */
case class Actor[Context](
    beh: Behavior,
    env: Environment[Address],
    ctx: Context)
    extends SchemeModActorComponent[Context]:

    def removeContext: SchemeModActorComponent[Unit] = Actor(beh, env, ())
    def removeEnv: Actor[Context] = Actor(beh.copy(lexEnv = Environment.empty), Environment.empty, ctx)

trait StandardSchemeModActorComponents extends SchemeModActorSemantics:
    type Component = SchemeModActorComponent[Ctx]
    lazy val initialComponent = MainActor
    def newComponent(cmp: Actor[Ctx]) = cmp
    def view(cmp: Component) = cmp

/** The analysis of an actor consists of analyzing all the components within that actor */
case class ActorAnalysisComponent[Ctx](enclosingActor: SchemeModActorComponent[Unit], innerComponent: Option[SchemeModFComponent], ctx: Option[Ctx])
    extends SchemeModActorComponent[Ctx]:

    override def removeEnv: SchemeModActorComponent[Ctx] =
        this.copy(enclosingActor = enclosingActor.removeEnv)

    override def removeContext: SchemeModActorComponent[Unit] =
        this.copy(ctx = None)

/**
 * A component that represents the analysis of a particular behavior
 *
 * @param beh
 *   the behavior to analyze
 * @param env
 *   the environment to analyze the behavior in. It should be extended with the bindings needed for the evaluation of the body of the behavior
 * @param ctx
 *   an additional context that can be used to change the precision of the analysis. Otherwise the syntactical position of the behavior is the only
 *   discriminator.
 */
case class BehaviorComponent[Ctx](beh: Behavior, env: Environment[Address], ctx: Ctx) extends SchemeModFComponent
