package maf.modular.scheme.modactor

import maf.core.DynMonad
import maf.modular.scheme.modflocal.{EffectLens, EffectsM, EffectsMC}
import maf.language.AScheme.*
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
import maf.core
import maf.core.MaybeEq
import maf.modular.ReturnAddr
import maf.lattice.interfaces.BoolLattice
import maf.core.worklist.WorkList

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
trait SchemeModActorSemantics extends ModAnalysis[SchemeExp] with ASchemeDomain:
    inter =>
    given Logger.Logger = Logger.DisabledLog()

    import maf.util.LogOps.*

    type Component <: AID
    def actorIdComponent(a: AID)(using ClassTag[Component]): Component = a match
        case b: Component => b
        case _            => throw new Exception("not a properly formatted actor id")

    //
    // Methods to view and inject standard components in the components of the user's chosing
    //
    type ComponentContext

    def initialComponent: Component
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

    //
    // addresses
    //
    case class ActorWaitFutureResolveAddr(actor: Component) extends Address:
        def idn: Identity = Identity.none
        def printable: Boolean = true

    //
    // Messages
    //

    type Msg
    // TODO: remove these methods, in favor of the one in the monad
    def mkMessage(tpy: String, arguments: List[Value]): Msg
    def getTag(msg: Msg): String
    def getArgs(msg: Msg): List[Value]

    //
    // Mailboxes
    //
    type Mailbox = AbstractMailbox[Msg]

    def emptyMailbox: Mailbox

    def getMailboxes: Map[Component, Mailbox] = ???
    def getBehaviors: Set[Behavior] = ???

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
        // Enfore that the lattice is the same as the inter-actor semantics
        override type Value = inter.Value
        override lazy val lattice = inter.lattice

        override type Message = Msg

        /** The type of the internal state of the Monad */
        type State

        /** Needed state transformations */
        given lens: ActorLens[State]
        trait ActorLens[S] extends Lens[S], EffectLens[Component, S]:
            //
            // Store
            //
            def putSto(st: S, sto: Map[Address, Value]): S
            def getSto(st: S): Map[Address, Value]
            def sto = (putSto, getSto)

            //
            // Mailboxes
            //
            def putMailboxes(st: S, mb: Map[Component, Mailbox]): S
            def getMailboxes(st: S): Map[Component, Mailbox]
            def mailboxes = (putMailboxes, getMailboxes)

            //
            // Set of actors spawned
            //
            def putActors(st: S, actors: Set[Component]): S
            def getActors(st: S): Set[Component]
            def actors = (putActors, getActors)

            //
            // Set of behaviors discovered during the sequential intra-analysis
            //
            def putBehaviors(st: S, behs: Set[Behavior]): S
            def getBehaviors(st: S): Set[Behavior]
            def behaviors = (putBehaviors, getBehaviors)

        implicit override val analysisM: ModularAnalysisM

        /** A monad that supports mondular analysis semantics needs to have a notion of "effects" */
        trait ModularAnalysisM extends ActorAnalysisM[A], EffectsM[A, Component, State], StateOps[State, A]:
            //
            // Abstract methods
            //

            def allocConstructorVar(actor: Component)(par: Identifier): A[Adr]

            /* Returns the component associated with the entrypoint of the current component */
            def selfActorCmp: A[Component]

            /** Allocate a new component for the given actor */
            def allocateActor(initialBehavior: Behavior, idn: Identity): A[Component]

            /** Allocate a ModF call component */
            def allocateCall(lam: Lam, env: Environment[Address], ctx: Ctx): A[Component]

            /** Allocat a component for the new behavior */
            def allocateBehavior(beh: Behavior, idn: Identity): A[Component]

            given componentGiven: ClassTag[Component]

            override def spawnActor(beh: Behavior, ags: List[Value], idn: Identity): A[ActorRef] = for
                cmp <- allocateActor(beh, idn)
                // allocate actor arguments
                _ <- beh.prs.mapM(allocConstructorVar(cmp)).flatMap(adrs => extendSto(adrs.zip(ags)))
                // spawn the actor
                _ <- spawn(cmp)
                // put the newly spawned actor in the list of actors
                _ <- get.map(lens.modify(lens.actors)(_ + cmp)) >>= put
            yield (maf.language.AScheme.ASchemeValues.Actor(beh.name, cmp))

            override def become(beh: Behavior, ags: List[Value], idn: Identity): A[Unit] = for
                // put the behavior arguments in the store
                _ <- beh.prs.mapM(this.allocVar).flatMap(adrs => extendSto(adrs.zip(ags)))
                // track the created behaviors
                _ <- get.map(lens.modify(lens.behaviors)(_ + beh)) >>= put
                // create a component for the behavior and spawn it
                _ <- allocateBehavior(beh, idn) >>= spawn
            yield ()

            override def addrEq: A[MaybeEq[Adr]] = unit(new MaybeEq[Adr] {
                override def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
                    BoolLattice[B].inject(a1 == a2)
            })

            override def getMessageTag(m: Message): String =
                inter.getTag(m)

            override def mkMessage(tpy: String, arguments: List[Value]): A[Message] =
                unit(inter.mkMessage(tpy, arguments))

            override def getMessageArguments(m: Message): List[Val] =
                inter.getArgs(m)

            override def call(lam: Lam): A[Val] =
                for
                    env <- getEnv
                    ctx <- getCtx
                    _ <- allocateCall(lam, env, ctx) >>= spawn
                    cmp <- selfCmp
                    result <- lookupSto(ReturnAddr(cmp, lam.idn))
                yield result

            def updateSto(a: Adr, v: Val): A[Unit] = extendSto(a, v)

            def extendSto(a: Adr, v: Val): A[Unit] =
                /* trigger a write */ trigger(AddrDependency(a)) >>> /* and actually do the write */
                    (get.map(lens.modify(lens.sto)(sto => sto + (a -> lattice.join(sto.get(a).getOrElse(lattice.bottom), v)))) >>= put)

            def lookupSto(a: Adr): A[Val] =
                register(AddrDependency(a)) >>>
                    get.map(lens.getSto).flatMap(_.get(a).map(unit).getOrElse(mbottom))

            def send(to: ActorRef, m: Message): A[Unit] =
                val receiver = actorIdComponent(to.tid)
                // A message send stores the message in the receiver's mailbox and triggers a re-analysis of the receiver
                trigger(MailboxDep(receiver)) >>>
                    (get.map(lens.modify(lens.mailboxes)(mbs => mbs + (receiver -> mbs.apply(receiver).enqueue(m)))) >>= put)

            def mailbox: A[Mailbox] =
                get.map(lens.getMailboxes).flatMap(boxes => selfCmp.map(boxes.apply))

            def receive: A[Message] =
                for
                    // register receive
                    cmp <- selfActorCmp
                    _ <- register(MailboxDep(cmp))
                    // get the mailbox in order to dequeue a message from it
                    mb <- mailbox
                    ms <- nondets(mb.dequeue.map(unit))
                    (msg, mb1) = ms
                    // save the resulting mailbox
                    _ <- get.map(lens.modify(lens.mailboxes)(_ + (cmp -> mb1))) >>= put
                // return the dequeued message
                yield msg

    val sequentialSemantics: ModularASchemeSemantics

    type Inter
    type Intra = sequentialSemantics.State

    def syncInter(vlu: Value, intra: Intra, inter: Inter): Inter
    def injectInter(inter: Inter, cmp: Component): DynMonad[Value, EffectsMC[Component, Intra]]
    val emptyWorklist: WorkList[Component]
    val initialInterState: Inter

    abstract class ModActorIntra(cmp: Component)(using ClassTag[Component]) extends IntraAnalysis(cmp):
        outer =>

        override def analyzeWithTimeout(timeout: Timeout.T): Unit =
            val _timeout = timeout
            EffectsM.fixWL[Component, Intra, Inter, Value](
              cmp,
              new EffectsM.Configuration {
                  def inject(inter: Inter, cmp: Component): DynMonad[Value, EffectsMC[Component, Intra]] =
                      injectInter(inter, cmp)
                  val emptyWL = inter.emptyWorklist
                  def sync(vlu: Value, intra: Intra, inter: Inter): Inter = syncInter(vlu, intra, inter)
                  val initialState: Inter = inter.initialInterState
                  val timeout = _timeout
              }
            )
