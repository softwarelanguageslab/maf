package maf.modular.scheme.modactor

import maf.util.*
import maf.language.CScheme.*
import maf.language.scheme.*
import maf.core.*
import maf.language.AScheme.ASchemeValues.AID

sealed trait SchemeModActorComponent[+Context] extends SmartHash with AID

// The main actor of the program
case object MainActor extends SchemeModActorComponent[Nothing]

/*
 * An actor that was spawned in some other actor.
 *
 * @param beh the initial behaviour of the actor
 * @param env the initial environment of the actor
 * @param ctx an optional context, to be used to change the sensitivity of the analysis
 */
case class Actor[Context](
    beh: SchemeExp,
    env: Environment[Address],
    ctx: Context)
    extends SchemeModActorComponent[Context]

trait StandardSchemeModActorComponents extends SchemeModActorSemantics:
    type Component = SchemeModActorComponent[ComponentContext]
    lazy val initialComponent = MainActor
    def newComponent(cmp: Actor[ComponentContext]) = cmp
    def view(cmp: Component) = cmp
