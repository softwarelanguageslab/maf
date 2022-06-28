package maf.modular.scheme.modactor

import maf.util.*
import maf.language.CScheme.*
import maf.language.scheme.*
import maf.core.*
import maf.language.AScheme.ASchemeValues.AID
import maf.language.AScheme.ASchemeValues.Behavior

sealed trait SchemeModActorComponent[+Context] extends SmartHash with AID:
    def removeContext: SchemeModActorComponent[Unit]
    def removeEnv: SchemeModActorComponent[Context]

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
    type Component = SchemeModActorComponent[ComponentContext]
    lazy val initialComponent = MainActor
    def newComponent(cmp: Actor[ComponentContext]) = cmp
    def view(cmp: Component) = cmp
