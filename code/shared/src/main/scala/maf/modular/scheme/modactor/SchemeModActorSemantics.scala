package maf.modular.scheme.modactor

import maf.modular.ModAnalysis
import maf.language.scheme.SchemeExp
import maf.modular.scheme.modf.BaseSchemeModFSemanticsM
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.language.AScheme.ASchemeValues
import maf.modular.scheme.modf.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.language.AScheme.ASchemeValues.AID
import scala.reflect.ClassTag
import maf.modular.Dependency
import maf.core.{Address, Environment, Identifier, Identity, Lattice, Monad, MonadStateT}
import maf.core.SetMonad.*
import maf.util.benchmarks.Timeout
import maf.modular.scheme.modf.TEvalM
import maf.language.scheme.ASchemeActor
import maf.language.AScheme.ASchemeValues.Behavior
import maf.language.scheme.ASchemeCreate
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.language.scheme.ASchemeSend
import maf.language.scheme.ASchemeBecome

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
trait SchemeModActorSemantics extends ModAnalysis[SchemeExp] with SchemeSetup:
    inter =>
    type Component <: AID
    def actorIdComponent(a: AID)(using ClassTag[Component]): Component = a match
        case b: Component => b
        case _            => throw new Exception("not a properly formatted actor id")

    //
    // Methods to view and inject standard components in the components of the user's chosing
    //
    type ComponentContext

    def initialComponent: Component
    def newComponent(actor: Actor[ComponentContext]): Component
    def view(cmp: Component): SchemeModActorComponent[ComponentContext]

    //
    // Analysis bodies and environments
    //
    type Env = Environment[Address]

    def body(cmp: Component) = view(cmp) match
        case MainActor =>
            this.program
        case Actor(beh, _, _) =>
            beh

    def env(cmp: Component): Env = env(view(cmp))
    def env(cmp: SchemeModActorComponent[ComponentContext]): Env = cmp match
        case MainActor        => initialEnv
        case Actor(_, env, _) => env

    //
    // allocating addresses
    //

    type AllocationContext
    def allocVar(
        id: Identifier,
        modFCmp: SchemeModFComponent,
        cmp: Component
      ): VarAddr[AllocationContext]
    def allocPtr(
        exp: SchemeExp,
        modFCmp: SchemeModFComponent,
        cmp: Component
      ): PtrAddr[AllocationContext]

    //
    // Messages
    //

    type Msg

    def mkMessage(tpy: String, arguments: List[Value]): Msg

    //
    // Mailboxes
    //
    type Mailbox = AbstractMailbox[Msg]

    def emptyMailbox: Mailbox

    /**
     * Each actor keeps track of each own mailbox. A single actor corresponds to a single component in our analysis, therefore we map components to
     * mailboxes.
     */
    protected var mailboxes: Map[Component, Mailbox] = Map().withDefaultValue(emptyMailbox)

    //
    // Inter analysis
    //

    case class MailboxDep(cmp: Component) extends Dependency

    //
    // Intra analysis
    //

    override def intraAnalysis(component: Component): ModActorIntra
    trait ModActorIntra(cmp: Component)(using ClassTag[Component]) extends IntraAnalysis with GlobalStoreIntra with ReturnResultIntra:
        /** Send the given message to the given actor */
        def send(to: ASchemeValues.Actor, m: Msg): Unit =
            // compute the component that needs to receive this message
            val toComponent = actorIdComponent(to.tid)
            // update the mailbox
            val oldMailbox = mailboxes(toComponent)
            val newMailbox = oldMailbox.enqueue(m)

            if oldMailbox != newMailbox then trigger(MailboxDep(actorIdComponent(to.tid)))

        /** Get access to our mailbox */
        def receive(): Mailbox =
            register(MailboxDep(component))
            mailboxes(component)

        /** Spawn an actor with the given initial behavior */
        def spawn(beh: Behavior, pos: Identity): Component = ???

    //
    // Inner ModF intra-process
    //

    abstract class InnerModF(intra: ModActorIntra)
        extends ModAnalysis[SchemeExp](body(intra.component))
        with SchemeModActorInnerMonad[Msg]
        with StandardSchemeModFComponents { modf =>

        import evalM.*
        import maf.core.Monad.MonadSyntaxOps
        import maf.core.Monad.MonadIterableOps

        override def intraAnalysis(component: modf.Component): InnerModFIntra = InnerModFIntra(component)

        // SCHEME ENVIRONMENT SETUP
        lazy val baseEnv = env(intra.component)
        // SCHEME LATTICE SETUP
        type Value = inter.Value
        lazy val lattice = inter.lattice
        lazy val primitives = inter.primitives
        // MODF ADDRESS ALLOCATION
        type AllocationContext = inter.AllocationContext
        def allocVar(id: Identifier, cmp: SchemeModFComponent) = inter.allocVar(id, cmp, intra.component)
        def allocPtr(exp: SchemeExp, cmp: SchemeModFComponent) = inter.allocPtr(exp, cmp, intra.component)
        // GLOBAL STORE SETUP
        override def store = intra.store
        override def store_=(s: Map[Addr, Value]) = intra.store = s
        // SYNCING DEPENDENCIES
        override def register(target: modf.Component, dep: Dependency) =
            super.register(target, dep)
            intra.register(dep)
        override def trigger(dep: Dependency) =
            super.trigger(dep)
            intra.trigger(dep)

        class InnerModFIntra(component: modf.Component) extends IntraAnalysis(component) with BigStepModFIntraT:
            val initialState: State = State(fnEnv, intra.receive())

            // analysis entry point
            def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
                eval(fnBody).run(initialState).foreach { case (vlu, _) => writeResult(vlu) }

            override def eval(exp: SchemeExp): EvalM[Value] = exp match
                // An actor expression evaluates to a behavior
                case ASchemeActor(parameters, selection, _, name) =>
                    unit(lattice.beh(Behavior(name, parameters, selection)))
                case ASchemeCreate(beh, ags, idn) =>
                    for
                        evaluatedBeh <- eval(beh)
                        evaluatedAgs <- ags.mapM(eval)
                        actorRef <- spawnActor(evaluatedBeh, evaluatedAgs, idn)
                    yield actorRef
                case ASchemeSend(actorRef, messageTpy, ags, idn) =>
                    for
                        evaluatedActorRef <- eval(actorRef)
                        evaluatedAgs <- ags.mapM(eval)
                        msg = mkMessage(messageTpy.name, evaluatedAgs)
                        _ <- nondets(lattice.getActors(evaluatedActorRef).map { actor =>
                            intra.send(actor, msg)
                            unit(())
                        })
                    yield lattice.nil

                case ASchemeBecome(beh, ags, idn) =>
                    ???

                case _ => super.eval(exp)

            def spawnActor(beh: Value, ags: List[Value], idn: Identity): EvalM[Value] =
                nondets(
                  lattice
                      .getBehs(beh)
                      .map(beh =>
                          // spawn the actor
                          val cmp = intra.spawn(beh, idn)
                          // write the arguments of the actor
                          beh.prs.zip(ags).foreach { case (par, arg) =>
                              val addr = inter.allocVar(par, Main, cmp)
                              writeAddr(addr, arg)
                          }

                          unit(lattice.actor(ASchemeValues.Actor(beh.name, cmp)))
                      )
                )
    }
