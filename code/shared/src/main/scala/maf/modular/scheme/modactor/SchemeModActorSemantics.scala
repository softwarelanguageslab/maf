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
import maf.modular.AnalysisEntry
import maf.util.benchmarks.Timeout.T

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
abstract class SchemeModActorSemantics(val program: SchemeExp) extends AnalysisEntry[SchemeExp] with ASchemeDomain with ASchemeSemantics:
    inter =>

    import maf.util.LogOps.*

    type Component <: AID
    def actorIdComponent(a: AID)(using ClassTag[Component]): Component

    /** Returns the enclosing actor of this component */
    protected def enclosing(cmp: Component): Component

    //
    // Methods to view and inject standard components in the components of the user's chosing
    //

    def initialComponent: Component
    def view(cmp: Component): SchemeModActorComponent[Ctx]
    def componentContext(cmp: Component): Ctx
    def environment(cmp: Component): Env

    //
    // Analysis bodies and environments
    //

    def expr(cmp: Component): SchemeExp = body(cmp)
    def body(cmp: Component): SchemeExp

    //
    // Addresses
    //
    case class ActorWaitFutureResolveAddr(actor: Component) extends Address:
        def idn: Identity = Identity.none
        def printable: Boolean = true

    //
    // Messages
    //

    // TODO: remove these methods, in favor of the one in the monad
    def mkMessage(tpy: String, arguments: List[Value]): Msg
    def getTag(msg: Msg): String
    def getArgs(msg: Msg): List[Value]

    //
    // Mailboxes
    //
    type Mailbox = AbstractMailbox[Msg]

    def emptyMailbox: Mailbox

    def getMailboxes: Map[Component, Mailbox]
    def getBehaviors: Map[Component, Set[Behavior]]

    //
    // Inter analysis
    //

    case class MailboxDep(cmp: Component) extends Dependency

    /////////////////////////////////////////////////////////////
    // Semantics
    /////////////////////////////////////////////////////////////

    /** The type of the internal state of the Monad */
    type State

    /////////////////////////////////////////////////////////////
    // State management
    /////////////////////////////////////////////////////////////

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
        def putBehaviors(st: S, behs: Map[Component, Set[Behavior]]): S
        def getBehaviors(st: S): Map[Component, Set[Behavior]]
        def behaviors = (putBehaviors, getBehaviors)

    implicit override val analysisM: ModularAnalysisM

    /** A monad that supports mondular analysis semantics needs to have a notion of "effects" */
    trait ModularAnalysisM extends ActorAnalysisM[A], EffectsM[A, Component, State], StateOps[State, A]:
        //
        // Abstract methods
        //

        /* Returns the component associated with the entrypoint of the current component */
        def selfActorCmp: A[Component]

        /** Allocate a new component for the given actor */
        def allocateActor(initialBehavior: Behavior, idn: Identity): A[Component]

        /** Allocate an empheral child for the given actor and message */
        def allocateEmpheralChild(component: Component, m: Msg): A[Component]

        /** Allocate a ModF call component */
        def allocateCall(lam: Lam, env: Environment[Address]): A[Component]

        /** Allocat a component for the new behavior */
        def allocateBehavior(beh: Behavior, idn: Identity): A[Component]

        given componentGiven: ClassTag[Component]

        override def spawnActor(beh: Behavior, ags: List[Value], idn: Identity): A[ActorRef] = for
            adrs <- beh.prs.mapM(allocVar(_))
            // Allocate the component in the correct environment
            cmp <-
                withEnv(_ => beh.lexEnv.extend(beh.prs.map(_.name).zip(adrs))) {
                    allocateActor(beh, idn)
                }
            // allocate actor arguments
            _ <- beh.prs.mapM(allocVar(_)).flatMap(adrs => extendSto(adrs.zip(ags)))
            // spawn the actor
            _ <- spawn(cmp)
            // put the newly spawned actor in the list of actors
            _ <- get.map(lens.modify(lens.actors)(_ + cmp)) >>= put
        yield (maf.language.AScheme.ASchemeValues.Actor(beh.name, cmp))

        override def become(beh: Behavior, ags: List[Value], idn: Identity): A[Unit] =
            for
                adrs <- beh.prs.mapM(this.allocVar)
                // put the behavior arguments in the store
                _ <- extendSto(adrs.zip(ags))
                // get a reference to ourselves
                cmp <- selfActorCmp
                // track the created behaviors
                _ <- get.map(lens.modify(lens.behaviors)(m => m + (cmp -> (m.get(cmp).getOrElse(Set()) + beh)))) >>= put
                // create a component for the behavior and spawn it
                _ <- withEnv(_ => beh.lexEnv.extend(beh.prs.map(_.name).zip(adrs))) { allocateBehavior(beh, idn) } >>= spawn
            yield ()

        override def addrEq: A[MaybeEq[Adr]] = unit(new MaybeEq[Adr] {
            override def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
                BoolLattice[B].inject(a1 == a2)
        })

        override def getMessageTag(m: Msg): String =
            inter.getTag(m)

        override def mkMessage(tpy: String, arguments: List[Value]): A[Msg] =
            unit(inter.mkMessage(tpy, arguments))

        override def getMessageArguments(m: Msg): List[Val] =
            inter.getArgs(m)

        override def call(lam: Lam): A[Val] =
            for
                env <- getEnv
                ctx <- getCtx
                newCmp <- allocateCall(lam, env)
                _ <- spawn(newCmp)
                result <- lookupSto(ReturnAddr(newCmp, Identity.none))
            yield result

        def updateSto(a: Adr, v: Val): A[Unit] = extendSto(a, v)

        def extendSto(a: Adr, v: Val): A[Unit] =
            val doTrigger =
                trigger(AddrDependency(a))
            (get.map(lens.modify(lens.sto)(sto => sto + (a -> lattice.join(sto.get(a).getOrElse(lattice.bottom), v))))
                >>= (putDoIfChanged(doTrigger)))

        def lookupSto(a: Adr): A[Val] =
            register(AddrDependency(a)) >>>
                get.map(lens.getSto).flatMap(_.get(a).map(unit).getOrElse(mbottom))

        def send(to: ActorRef, m: Msg): A[Unit] =
            val receiver = actorIdComponent(to.tid)
            // A message send stores the message in the receiver's mailbox and triggers a re-analysis of the receiver
            //
            (get.map(
              lens.modify(lens.mailboxes)(mbs =>
                  mbs + (receiver ->
                      mbs.get(receiver).getOrElse(emptyMailbox).enqueue(m))
              )
            ) >>= putDoIfChanged { trigger(MailboxDep(receiver)) })

        def ask(to: ActorRef, m: Msg): A[Value] =
            for
                // create the empheral child
                self <- selfActorCmp
                empheralChild <- allocateEmpheralChild(self, m)
                // read the result from the mailbox
                _ <- register(MailboxDep(empheralChild))
                mb <- get.map(lens.getMailboxes).map(_.get(empheralChild).getOrElse(emptyMailbox))
                ms <- nondets(mb.dequeue.map(unit))
                (msg, mb1) = ms
                _ <- get.map(lens.modify(lens.mailboxes)(_ + (empheralChild -> mb1))) >>= put
                tag = getMessageTag(msg)
                vlus = getMessageArguments(msg)
                // make sure that the tag is a receive
                _ <- guard(tag == "reply" && vlus.size == 1)
            yield vlus.head

        def mailbox: A[Mailbox] =
            get.map(lens.getMailboxes).flatMap(boxes => selfCmp.map(enclosing).map(boxes.get(_)).map(_.getOrElse(emptyMailbox)))

        def receive: A[Msg] =
            for
                // register receive
                cmp <- selfCmp map enclosing
                _ <- register(MailboxDep(cmp))
                // get the mailbox in order to dequeue a message from it
                mb <- mailbox
                ms <- nondets(mb.dequeue.map(unit))
                (msg, mb1) = ms
                // save the resulting mailbox
                _ <- get.map(lens.modify(lens.mailboxes)(_ + (cmp -> mb1))) >>= put
            // return the dequeued message
            yield msg

    type Inter
    type Intra = State

    def syncInter(intra: Intra, inter: Inter): Inter
    def injectInter(inter: Inter, cmp: Component): DynMonad[Value, EffectsMC[Component, Intra]]
    val emptyWorklist: WorkList[Component]
    lazy val initialInterState: Inter

    //////////////////////////////////////////////////////////////
    // Auxilary functions for initializing the correct environment
    //////////////////////////////////////////////////////////////

    lazy val initialEnv: Env
    lazy val initialSto: Map[Adr, Val]

    ////////////////////////////////
    // AnalysisEntry
    ////////////////////////////////

    private var isFinished: Boolean = false
    protected var _result: Inter | Null = null

    def finished: Boolean = isFinished
    override def result: Option[Any] = Some(_result)

    def analyzeWithTimeout(_timeout: T): Unit =
        val result = EffectsM.fixWL[Component, Intra, Inter, Value](
          initialComponent,
          new EffectsM.Configuration {
              def inject(inter: Inter, cmp: Component): DynMonad[Value, EffectsMC[Component, Intra]] =
                  injectInter(inter, cmp)
              val emptyWL = inter.emptyWorklist
              def sync(intra: Intra, inter: Inter): Inter = syncInter(intra, inter)
              val initialState: Inter = inter.initialInterState
              val timeout = _timeout
          }
        ) match
            case EffectsM.AnalysisResult.Finished(inter) => { isFinished = true; inter }
            case EffectsM.AnalysisResult.Timeout(inter)  => { isFinished = false; inter }

        _result = result
