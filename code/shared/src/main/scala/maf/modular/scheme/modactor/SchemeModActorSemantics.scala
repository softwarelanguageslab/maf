package maf.modular.scheme.modactor

import maf.modular.scheme.modflocal.{EffectLens, EffectsM}
import maf.modular.ModAnalysis
import maf.util.*
import maf.core.monad.*
import maf.language.scheme.*
import maf.modular.scheme.modf.BaseSchemeModFSemanticsM
import maf.util.datastructures.MapOps.*
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.language.AScheme.ASchemeValues
import maf.modular.scheme.modf.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.language.AScheme.ASchemeValues.{AID, EmptyBehavior}
import scala.reflect.ClassTag
import maf.modular.Dependency
import maf.core.{Address, Environment, Identifier, Identity, Lattice, Monad, MonadStateT}
import maf.core.SetMonad.*
import maf.core.Monad.*
import maf.util.benchmarks.Timeout
import maf.modular.scheme.modf.TEvalM
import maf.language.scheme.ASchemeActor
import maf.language.AScheme.ASchemeValues.Behavior
import maf.language.scheme.ASchemeCreate
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.language.scheme.ASchemeSend
import maf.language.scheme.ASchemeBecome
import java.util.concurrent.TimeoutException
import maf.language.scheme.ASchemeSelect
import maf.util.FunctionUtils.fixWL
import maf.util.Logger
import maf.util.FunctionUtils.FIFOWL
import maf.util.datastructures.MapOps.MapWithDefault
import maf.language.scheme.lattices.SchemeLattice
import maf.language.AScheme.ASchemeLattice
import maf.language.scheme.primitives.StoreM
import maf.core.StateOps
import maf.modular.AddrDependency

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
    given Logger.Logger = Logger.DisabledLog()

    import maf.util.LogOps.*

    implicit override lazy val lattice: ASchemeLattice[Value, Address]

    type Component <: AID
    def actorIdComponent(a: AID)(using ClassTag[Component]): Component = a match
        case b: Component => b
        case _            => throw new Exception("not a properly formatted actor id")

    //
    // Methods to view and inject standard components in the components of the user's chosing
    //
    type ComponentContext

    def allocCtx(currCmp: Component, idn: Identity): ComponentContext
    def initialComponent: Component
    def newComponent(actor: Actor[ComponentContext]): Component
    def view(cmp: Component): SchemeModActorComponent[ComponentContext]

    //
    // Analysis bodies and environments
    //
    type Env = Environment[Address]

    def expr(cmp: Component): SchemeExp = body(cmp)

    def body(cmp: Component): SchemeExp = view(cmp) match
        case MainActor =>
            this.program
        case Actor(beh, _, _) =>
            beh.bdy

    def env(cmp: Component): Env = env(view(cmp))
    def env(cmp: SchemeModActorComponent[ComponentContext]): Env = cmp match
        case MainActor        => initialEnv
        case Actor(_, env, _) => env

    //
    // addresses
    //
    case class ActorWaitFutureResolveAddr(actor: Component) extends Address:
        def idn: Identity = Identity.none
        def printable: Boolean = true

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
    // TODO: remove these methods
    def mkMessage(tpy: String, arguments: List[Value]): Msg
    def getTag(msg: Msg): String
    def getArgs(msg: Msg): List[Value]

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
    def getMailboxes: Map[Component, Mailbox] = mailboxes

    //
    // Behaviors
    //

    /**
     * We keep track of the set of discovered behaviors for each component. This is mainly to support some client analyses, and the precision and
     * soundness tests
     */
    protected var behaviors: MapWithDefault[Component, Set[Behavior]] = Map().useDefaultValue(Set())
    def getBehaviors: Map[Component, Set[Behavior]] = behaviors

    //
    // Inter analysis
    //

    case class MailboxDep(cmp: Component) extends Dependency

    //
    // Intra analysis
    //

    override def intraAnalysis(component: Component): ModActorIntra

    //
    // Semantics
    //
    trait ModularASchemeSemantics extends ASchemeSemantics:
        val messageM: MessageM[A]
        val storeM: StoreM[A, Adr, Val]
        val effectsM: EffectsM[A, Component, State]

        // Enfore that the lattice is the same as the inter-actor semantics
        override type Value = inter.Value
        override lazy val lattice = inter.lattice

        /** The result of the internal computation of the analysis monad */
        type Result

        /** The type of the internal state of the Monad */
        type State

        /** Needed state transformations */
        val lens: ActorLens[State]
        trait ActorLens[S] extends Lens[S], EffectLens[S]:
            def putSto(st: S, sto: Map[Address, Value]): S
            def getSto(st: S): Map[Address, Value]
            def sto = (putSto, getSto)
            def putMailboxes(st: S, mb: Map[Component, Mailbox]): S
            def getMailboxes(st: S): Map[Component, Mailbox]
            def mailboxes = (putMailboxes, getMailboxes)

        /** Run the monad until completion, returning its state and value */
        def run[X](m: A[X]): A[(X, Result)]

        implicit override val analysisM: ModularAnalysisM

        /** A monad that supports mondular analysis semantics needs to have a notion of "effects" */
        trait ModularAnalysisM extends ActorAnalysisM[A], EffectsM[A, Component, State], StateOps[State, A]:
            def extendSto(a: Adr, v: Val): A[Unit] =
                /* trigger a write */ trigger(a) >>> /* and actually do the write */
                    (get.map(lens.modify(lens.sto)(sto => sto + (a -> lattice.join(sto.get(a).getOrElse(lattice.bottom), v)))) >>= put)

            def lookupSto(a: Adr): A[Val] =
                register(AddrDependency(a)) >>>
                    get.map(lens.getSto).flatMap(_.get(a).map(unit).getOrElse(mbottom))

            def send(to: ActorRef, m: Message): A[Val] =
                // A message send stores the message in the receiver's mailbox and triggers a re-analysis of the receiver
                ???

    // TODO: val semantics: ModularASchemeSemantics

    class ModActorIntra(cmp: Component)(using ClassTag[Component]) extends IntraAnalysis(cmp) with GlobalStoreIntra with ReturnResultIntra:
        override def analyzeWithTimeout(timeout: Timeout.T): Unit =
            import maf.util.FunctionUtils.WL.*
            log("==========================================")
            log(s"analyzing $component")
            if timeout.reached then throw new TimeoutException()
            else
                // the intra analysis consists of a series of ModF inner analyses.
                val initialBehavior = view(cmp) match
                    case MainActor        => EmptyBehavior(body(cmp))
                    case Actor(beh, _, _) => beh

                // TODO: is transfer the correct terminology?
                def transfer(seenBehavior: FIFOWL[Behavior]): FIFOWL[Behavior] =
                    log(s"Transfer $seenBehavior")
                    seenBehavior.addNext { beh =>
                        val modf = innerModF(this, beh)
                        modf.analyzeWithTimeout(timeout)
                        modf.getBehaviors
                    }

                val behaviors = fixWL(FIFOWL.initial(initialBehavior))(transfer)

                inter.behaviors = inter.behaviors.update(cmp)(_ ++ behaviors)

        /** Send the given message to the given actor */
        def send(to: ASchemeValues.Actor, m: Msg): Unit =
            log(s"sending message $m to $to")
            // compute the component that needs to receive this message
            val toComponent = actorIdComponent(to.tid)
            // update the mailbox
            val oldMailbox = mailboxes(toComponent)
            val newMailbox = oldMailbox.enqueue(m)
            // update the global mailbox map
            mailboxes = mailboxes + (toComponent -> newMailbox)

            if oldMailbox != newMailbox then trigger(MailboxDep(actorIdComponent(to.tid)))

        /** Get access to our mailbox */
        def receive(): Mailbox =
            register(MailboxDep(component))
            mailboxes(component)

        override def doWrite(dep: Dependency): Boolean = dep match
            case MailboxDep(_) => true
            case _             => super.doWrite(dep)

        /** Spawn an actor with the given initial behavior */
        def spawn(beh: Behavior, lexEnv: Environment[Address], pos: Identity): Component =
            val ctx = allocCtx(component, pos)
            val cmp = newComponent(Actor(beh, lexEnv, ctx))
            super.spawn(cmp)
            cmp

        def actorIdComponent(aid: AID): Component = inter.actorIdComponent(aid)

        def notifyFutures(v: Value): Unit =
            writeAddr(ActorWaitFutureResolveAddr(cmp), v)

    //
    // Inner ModF intra-process
    //

    def innerModF(intra: ModActorIntra, beh: Behavior): InnerModF

    // TODO: rename to SequentialModFAnalysis? or SingleActorModFFactorySingletonProxyDefault?
    abstract class InnerModF(intra: ModActorIntra, beh: Behavior)
        extends ModAnalysis[SchemeExp](beh.bdy)
        with SchemeModActorInnerMonad[Msg]
        with StandardSchemeModFComponents { modf =>

        import evalM.*
        import maf.core.Monad.MonadSyntaxOps
        import maf.core.Monad.MonadIterableOps

        override def warn(msg: String): Unit =
            log(s"warn: $msg")

        override def intraAnalysis(component: modf.Component): InnerModFIntra =
            log(s"Analyzing $component")
            InnerModFIntra(component)

        // SCHEME ENVIRONMENT SETUP
        lazy val baseEnv = env(intra.component)
        // SCHEME LATTICE SETUP
        implicit override lazy val lattice: ASchemeLattice[Value, Address] = inter.lattice
        type Value = inter.Value
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

        private var behaviors: Set[Behavior] = Set()
        def getBehaviors: Set[Behavior] = behaviors

        class InnerModFIntra(component: modf.Component) extends IntraAnalysis(component) with BigStepModFIntraT:
            protected def recordNewBehavior(beh: Value, ags: List[Value]): EvalM[Unit] =
                nondets(lattice.getBehs(beh).map { beh =>
                    writeActorArgs(beh, ags, intra.component)
                    log(s"Recording new behavior $beh")
                    behaviors = behaviors + beh
                    unit(())
                })

            protected def writeActorArgs(beh: Behavior, ags: List[Value], actor: inter.Component): Unit =
                beh.prs.zip(ags).foreach { case (par, arg) =>
                    val addr = inter.allocVar(par, Main, actor)
                    writeAddr(addr, arg)
                }
            private def baseEnv: Environment[Address] = inter.view(intra.component) match
                case c: Actor[_] =>
                    val adr = allocVar(Identifier("self", Identity.none), component)
                    val adr2 = allocVar(Identifier("a/self", Identity.none), component)
                    writeAddr(adr, lattice.actor(ASchemeValues.Actor(c.beh.name, intra.component)))
                    writeAddr(adr2, lattice.actor(ASchemeValues.Actor(c.beh.name, intra.component)))
                    super.fnEnv.extend("self", adr).extend("a/self", adr2)
                case _ => super.fnEnv

            override def fnEnv: Environment[Address] = component match
                case Main =>
                    // inject the parameters of the behavior into the component
                    val prs = beh.prs
                    val ads = beh.prs.map(inter.allocVar(_, Main, intra.component))
                    baseEnv.extend(prs.map(_.name).zip(ads))
                case _ => baseEnv

            val initialState: State = State(fnEnv, intra.receive())

            protected def bindArgs(prs: List[Identifier], vlus: List[Value])(env: Environment[Address]): Environment[Address] =
                prs.zip(vlus).foldLeft(env) { case (env, (par, vlu)) =>
                    val addr = allocVar(par, component)
                    writeAddr(addr, vlu)
                    env.extend(par.name, addr)
                }

            // analysis entry point
            def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
                log(s"Analyzing behavior $beh with body $fnBody")
                eval(fnBody).run(initialState).foreach { case (vlu, _) => writeResult(vlu) }

            def spawnActor(beh: Value, ags: List[Value], idn: Identity): EvalM[Value] =
                log(s"Spawning actor with behavior $beh, ags $ags")
                nondets(
                  lattice
                      .getBehs(beh)
                      .map(beh =>
                          // spawn the actor
                          val cmp = intra.spawn(beh, beh.lexEnv, idn)
                          // write the arguments of the actor
                          writeActorArgs(beh, ags, cmp)
                          unit(lattice.actor(ASchemeValues.Actor(beh.name, cmp)))
                      )
                )
    }
