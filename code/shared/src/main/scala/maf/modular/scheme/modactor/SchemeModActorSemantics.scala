package maf.modular.scheme.modactor

import maf.modular.ModAnalysis
import maf.language.scheme.SchemeExp
import maf.modular.scheme.modf.BaseSchemeModFSemantics
import maf.modular.scheme.modf.BigStepModFSemantics
import maf.modular.scheme.modf.StandardSchemeModFComponents

/**
 * An implementation of ModConc for actors, as described in the following publication: Stiévenart, Quentin, et al. "A general method for rendering
 * static analyses for diverse concurrency models modular." Journal of Systems and Software 147 (2019): 17-45.
 *
 * Notes on the implementation:
 *   - We do not support message overloading, meaning that a single message should only be defined once for a single behavior.
 *
 * The analysis consists of two interleaving analyses: an intra-process analysis and an inter-process analysis. Similar to
 * <code>SchemeModConcSemantics</code> the intra-process semantics is defined in terms of a regular ModF analysis, which is run to completion (with an
 * optional timeout) to obtain the results of the intra-process code.
 *
 * The intra-process analysis therefore analyses the actor starting from its initial behavior. It analyses the program until all messages in the
 * mailbox have been processed.
 *
 * In terms of intra-process state, the following information is kept: a mailbox and a current environment.
 */
trait SchemeModActorSemantics extends ModAnalysis[SchemeExp]:
    override def intraAnalysis(component: Component): ModActorIntra

    //
    // Methods to view and inject standard components in the components of the user's chosing
    //
    type ComponentContext

    def initialComponent: Component
    def newComponent(actor: Actor[ComponentContext]): Component
    def view(cmp: Component): SchemeModActorComponent[ComponentContext]

    //
    // Analysis bodies
    //

    def body(cmp: Component) = view(cmp) match
        case MainActor        => ???
        case Actor(beh, _, _) => ???

    //
    // Messages
    //

    type M

    //
    // The type of mailbox used
    //
    type Mailbox <: AbstractMailbox[M]

    //
    // Inner ModF intra-process
    //

    abstract class InnerModF(intra: ModActorIntra)
        extends ModAnalysis[SchemeExp](body(intra.component))
        with BaseSchemeModFSemantics
        with BigStepModFSemantics
        with StandardSchemeModFComponents { modf => }

    trait ModActorIntra(cmp: Component) extends IntraAnalysis
