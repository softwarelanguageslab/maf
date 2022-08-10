package maf.language.AScheme.interpreter

import maf.language.scheme.interpreter.*
import maf.core.*
import maf.util.datastructures.MapOps.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues.*
import maf.language.change.CodeVersion.{New, Old, Version}
import maf.language.AScheme.ASchemeValues.*
import scala.collection.immutable.Queue
import maf.language.ContractScheme.ContractValues
import maf.language.AScheme.ASchemeValues
import maf.util.LogOps

class CPSASchemeInterpreter(
    cb: (Identity, Value) => Unit = (_, _) => (),
    io: IO = new EmptyIO(),
    stack: Boolean = false,
    cbA: ASchemeInterpreterCallback = ASchemeInterpreterCallback.EmptyCallback
  )(using maf.util.Logger.Logger)
    extends CPSSchemeInterpreter(cb, io, stack):

    import LogOps.*

    //
    // Continuations
    //

    /** Actor wait state */
    case class Wait(beh: ASchemeSelect, env: Env, aid: AID) extends Continuation:
        override def toString: String =
            val handlers = beh.handlers.keys.mkString(",")
            s"Wait($handlers)"

    /** Continuation that is applied when the actor reference of a send expression has been evaluated */
    case class SendC(send: ASchemeSend, env: Env, cc: Continuation) extends InternalContinuation

    /** Continuation that is applied during the evaluating of the actor arguments */
    case class SendArgsC(
        remainingArgs: List[SchemeExp],
        vlus: List[Value],
        actorRef: Actor,
        send: ASchemeSend,
        env: Env,
        cc: Continuation)
        extends InternalContinuation

    /**
     * Continuation that is applied after the actor behavior has been evaluated
     *
     * @param args
     *   the arguments of the become/create expression
     * @param env
     *   the current environment to evaluate the arguments of the expression
     * @param isBecome
     *   true if the expression is a become expression
     * @param idn
     *   the source code location of the become/create expression
     * @param cc
     *   the next continuation in the continuation stack
     */
    case class BecoCreaC(args: List[SchemeExp], env: Env, isBecome: Boolean, idn: Identity, cc: Continuation) extends InternalContinuation

    /** Continuation that is applied during the evaluation of a become/create expression */
    case class BecoCreaArgsC(
        args: List[SchemeExp],
        beh: Behavior,
        vlus: List[Value],
        env: Env,
        isBecome: Boolean,
        idn: Identity,
        cc: Continuation)
        extends InternalContinuation

    /** Continuation that is applied after the future is evaluated in an await expression */
    case class AwaiC(cc: Continuation) extends InternalContinuation

    /** Continuation that indicates that the actor is waiting for a future to resolve */
    case class WaitFuture(cc: Continuation) extends InternalContinuation

    /** Continuation that is applied after the argument of a wait-for-termination expression has been evaluated */
    case class WaiTer(env: Env, cc: Continuation) extends InternalContinuation

    /** Continuation that is applied when the argument of a terminate expression has been evaluated */
    case class TerC(id: AID) extends Continuation

    /** Bottom of continuation stack for an actor */
    case class ActC(id: AID) extends Continuation

    //
    // Values
    //
    import ConcreteASchemeValues.*

    type Actor = ConcreteActor

    /**
     * Converts a value from the ASchemeValue domain to the concrete scheme interpreter domain
     *
     * @param vlu
     *   the value from the ASchemeValue domain
     * @return
     *   a value from the concrete scheme interpreter domain
     */
    private def inject(vlu: ASchemeValue): Value =
        ConcreteActorValue(vlu)

    /** Ensures that the value is a behavior */
    private def ensureBehavior(vlu: Value): Behavior = vlu match
        case ConcreteActorValue(b: Behavior) => b
        case _                               => throw new Exception(s"$vlu is not a behavior")

    /** Ensures that the value is an actor */
    private def ensureActor(vlu: Value): Actor = vlu match
        case a: ConcreteActor => a
        case _                => throw new Exception(s"$vlu is not an actor reference")

    /** Ensures that the value is a future */
    private def ensureFuture(vlu: Value): Future = vlu match
        case ConcreteActorValue(f: Future) => f
        case _                             => throw new Exception(s"$vlu is not a future")

    /**
     * Ensures that the continuation is a WaitFuture continuation
     *
     * @param st
     *   a state that must be checked
     * @return
     *   the continuation after the WaitFuture (i.e., WaitFuture.cc)
     */
    private def ensureWaitFuture(st: State): Continuation = st match
        case Kont(_, WaitFuture(next)) => next
        case _                         => throw new Exception(s"$st is not a state containing a wait future continuation")

    //
    // Futures
    //

    extension (fut: Future)
        def isResolved: Boolean =
            resolvedFutures.contains(fut)

        def value: Value =
            resolvedFutures(fut)

        def value_=(vlu: Value): Unit =
            if fut.isResolved then throw new Exception(s"future $fut is already resolved")
            else resolvedFutures = resolvedFutures + (fut -> vlu)

    //
    // Interpreter structures
    //

    private type M = Message[Value]
    private type AID = SimpleActorId

    /** The set of mailboxes for each actor id */
    private var mailboxes: Map[AID, Queue[M]] = Map().withDefaultValue(Queue())

    /** A map from futures to the actors that are waiting for them */
    private var futureQueues: MapWithDefault[Future, Set[AID]] = Map().useDefaultValue(Set())

    /** A map from actors to futures that are waiting for the completion of the actor */
    private var waitingForTermination: MapWithDefault[AID, Set[Future]] = Map().useDefaultValue(Set())

    /** Futures created by the given actor (and that are not resolved yet) */
    private var createdFutures: MapWithDefault[AID, Set[Future]] = Map().useDefaultValue(Set())

    /** Other way around */
    private var createdFuturesRev: MapWithDefault[Future, Set[AID]] = Map().useDefaultValue(Set())

    /** A mapping from futures to their resolved value */
    private var resolvedFutures: Map[Future, Value] = Map()

    /** A set of suspended states for each actor. Usually, actors are only suspended when it is waiting for messages */
    private var suspended: Map[AID, State] = Map()

    /** The map from actor ids to concrete actor references */
    private var actors: Map[AID, Actor] = Map()

    /** Reference to the current actor state */
    //private var currentActor: ActorState = ActorState.none

    /** Counter to keep track of the last used actor id (the main actor is always zero) */
    private var currentActorId: Int = 0

    /** Allocate a fresh actor identifier */
    private def allocActorId(): AID =
        currentActorId = currentActorId + 1
        SimpleActorId(currentActorId)

    case class ConcreteAddr(a: Addr) extends Address:
        val idn: Identity = a._2.idn
        val printable: Boolean = true
    private def toConcretePair(addr: Address): Addr = addr match
        case ConcreteAddr(p) => p
        case _               => throw new Exception(s"unsupported address type $addr")

    implicit private def toBasicEnv(env: Environment[Address]): Env = env match
        case BasicEnvironment(content) => content.mapValues(adr => toConcretePair(adr)).toMap
        case _                         => throw new Exception("unsupported type of environment")
    implicit private def fromBasicEnv(env: Env): Environment[Address] =
        BasicEnvironment(env.mapValues(ConcreteAddr.apply).toMap)

    implicit def asSimpleActorId(aid: ASchemeValues.AID): SimpleActorId = aid.asInstanceOf[SimpleActorId]

    /**
     * Spawn an actor for the given behavior
     *
     * @param beh
     *   the initial behavior of the actor that needs to be spawned
     * @param ags
     *   the arguments that are associated with the initial behavior of the actor
     * @return
     *   an actor reference, that can be used to send messages to
     */
    private def spawn(beh: Behavior, ags: List[Value], idn: Identity): Actor =
        log(s"Spawning actor $beh with args $ags")
        val id = allocActorId()
        val actorRef = ConcreteActor(beh.name, id, beh, idn)
        // maintain the bookkeeping
        actors = actors + (id -> actorRef)
        cbA.spawn(actorRef)

        // extend the environment with bindings for the actor arguments
        val extendedEnv = extendEnv(
          Identifier("a/self", Identity.none) :: Identifier("self", Identity.none) :: beh.prs,
          actorRef :: actorRef :: ags,
          beh.lexEnv
        )

        // start the behavior of the actor
        work = work.enqueue(Step(beh.bdy, extendedEnv, ActC(id)))
        // return the actor reference
        actorRef

    /** Blocks the actor until it receives a message */
    private def waitForSelect(selection: ASchemeSelect, env: Env, cc: Continuation): State = cc match
        // A select may only happen if the actor does not have any other continuation, since that would violate the non-blocking principles of the receive
        case ActC(id) =>
            // is there a message in the actor's mailbox
            if mailboxes(id).isEmpty then
                log("suspending actor")
                // suspend if no messages for this actor
                suspended = suspended + (id -> (Kont(Value.Nil, Wait(selection, env, id))))
                scheduleNext()
                state
            else
                log("receiving message from mailbox")
                // continue if there is a message for this actor
                val (ms, mb) = mailboxes(id).dequeue
                mailboxes = mailboxes + (id -> mb)
                handleMessage(ms, selection, env, cc)
        case _ => throw new Exception(s"`select` must be the last expression at ${selection.idn}")

    private def popSuspended(id: AID): Option[State] =
        suspended.get(id) match
            case s @ Some(st) =>
                log(s"waking $id with $st")
                suspended = suspended - id
                s
            case None => None

    /** Handle the given message using the given select statement */
    private def handleMessage(ms: M, selection: ASchemeSelect, env: Env, cc: Continuation): State =
        selection.handlers.get(ms.tag) match
            case Some((prs, first :: rest)) =>
                val extendedEnv = extendEnv(prs, ms.vlus, env)
                val nextCc = if rest.nonEmpty then BegC(rest, extendedEnv, cc) else cc
                Step(first, extendedEnv, nextCc)
            case Some((prs, List())) =>
                Kont(Value.Nil, cc)
            case None => throw new Exception(s"no suitable handler for ${ms.tag}")

    /** Asynchronously sends a message to the given actor's mailbox */
    private def sendMessage(actorRef: Actor, tag: String, ags: List[Value], idn: Identity, cc: Continuation): State =
        if ags.exists { case ConcreteActorValue(fut: Future) => true; case _ => false } then throw FutureCannotBeSent(idn)

        val id = asSimpleActorId(actorRef.tid)
        val ms = Message(tag, ags)
        cbA.sendMessage(actors(id), ms, idn)
        log(s"sending message $ms to $actorRef")
        popSuspended(id) match
            case Some(Kont(_, Wait(selection, env, id))) =>
                // schedule the delivery of the message if the actor is already waiting
                work = work.enqueue(handleMessage(ms, selection, env, ActC(id)))
            case Some(st @ Kont(_, WaitFuture(_))) =>
                // the actor is waiting for a future, add it back to suspended state
                suspended = suspended + (id -> st)
            case None =>
                // the receiving actor is not waiting for a message
                // lets enqueue it in its mailbox
                mailboxes = mailboxes + (id -> mailboxes(id).enqueue(ms))
            case Some(st) =>
                throw new Exception(s"illegal waiting actor state ${st}")

        Kont(Value.Nil, cc)

    /** Ensures that the continuation is a singleton, and returns its actor id */
    private def ensureEmptyStack(cc: Continuation): AID = cc match
        case ActC(id) => id
        case EndC()   => SimpleActorId(0)
        case _        => throw new Exception(s"unexpected behavior $cc")

    /**
     * Walk through the continuation stack until the actor id is found
     *
     * @todo
     *   make this faster by keeping the actor id in the evaluation context instead
     */
    private def walkUntilActorId(cc: Continuation): AID = cc match
        case ActC(id)                => id
        case EndC()                  => SimpleActorId(0)
        case c: InternalContinuation => walkUntilActorId(c.cc)
        case _                       => throw new Exception(s"unexpected continuation $cc")

    /** Become a different behavior */
    private def become(beh: Behavior, ags: List[Value], cc: Continuation): State =
        // ensure that this is the last action we do
        val id = ensureEmptyStack(cc)

        // ensure that there are no running futures created by this actor (cf. Chocola: Composable Concurrency Language)
        if createdFutures.nonEmpty then throw FutureMustTerminateBeforeEndOfTurn

        // bookkeeping
        cbA.become(actors(id), beh)
        // enqueue the body of the behavior
        val extendedEnv = extendEnv(
          Identifier("a/self", Identity.none) :: Identifier("self", Identity.none) :: beh.prs,
          actors(id) :: actors(id) :: ags,
          beh.lexEnv
        )
        work = work.enqueue(Step(beh.bdy, extendedEnv, cc))

        // fairness: after an actor has performed a single turn, it should yield control to another actor
        scheduleNext()
        state

    /** Return a future that completes when the actor has terminated */
    private def waitForActorTermination(v: Value, cc: Continuation): State =
        val currentActorId = walkUntilActorId(cc)
        val actor = ensureActor(v)
        // create a future
        val fut = ActorWaitCompleteFuture(actor.tid)
        log(s"Resolved futures $resolvedFutures")
        // only do something if it is not yet resolved
        if !fut.isResolved then
            // add it to the queue of futures waiting for the actors completion
            waitingForTermination = waitingForTermination.update(actor.tid)(_ + fut)
            // add it to created futures
            createdFutures = createdFutures.update(currentActorId)(_ + fut)
            createdFuturesRev = createdFuturesRev.update(fut)(_ + currentActorId)
        else log(s"future $fut is already resolved, not adding it to the queue")
        // return it
        Kont(fut, cc)

    /** Resolves the futures that have been waiting for the given actor to terminate */
    private def resolveTerminationFuture(id: AID, v: Value): State =
        // add a fake waiting future, such that if the call to wait-for-termination is made after termination, it is still resolved
        val fakeWait = ActorWaitCompleteFuture(id)
        waitingForTermination = waitingForTermination.update(id)(_ + fakeWait)
        // resolve all the futures that have been waiting
        val waitingFutures = waitingForTermination(id)
        waitingFutures.foreach(resolveFuture(_, v))
        // then remove them from the list of waiting futures
        waitingForTermination = waitingForTermination.update(id)(_ => Set())
        // then return the state as determined by the last call to resolveFuture
        state

    /** Wait until the given future completes */
    private def awaitFuture(vlu: Value, cc: Continuation): State =
        log(s"awaiting future $vlu")
        val fut = ensureFuture(vlu)
        if fut.isResolved then Kont(fut.value, cc)
        else
            // we will need to wait until the future is resolved
            val actorId = walkUntilActorId(cc)
            futureQueues = futureQueues.update(fut)(_ + actorId)
            // add the actor to the state of suspended actors
            suspended = suspended + (actorId -> Kont(Value.Nil, WaitFuture(cc)))
            // then schedule another actor
            scheduleNext()
            state

    /** Resolves that given future to the given value, and wakens any waiting actors */
    private def resolveFuture(fut: Future, vlu: Value): State =
        log(s"resolving future $fut to value $vlu")
        // set the value of the future
        fut.value = vlu

        // it is possible that there are no waiting futures yet, only look for them if there are waiting actors in the queue
        if futureQueues.contains(fut) then
            // reschedule the execution of the waiting actors with the given value
            val waitingActors = futureQueues(fut)
            val waitingActorsStates = waitingActors.flatMap(suspended.get(_))
            val nextCCs = waitingActorsStates.map(ensureWaitFuture)
            work = nextCCs.foldLeft(work)((work, cc) => work.enqueue(Kont(vlu, cc)))

            // remove all woken actors from the future queue
            futureQueues = futureQueues.update(fut)(_ => Set())
            // remove all woken actors from the suspended actors
            suspended = suspended.removedAll(waitingActors)

        // remove the future from the actor that has created it, however it could be that is has not been created yet
        if createdFuturesRev.contains(fut) then
            val creationActors = createdFuturesRev(fut)
            createdFutures = creationActors.foldLeft(createdFutures)((createdFutures, actor) => createdFutures.update(actor)(_ - fut))
            createdFuturesRev = createdFuturesRev.removed(fut).useDefaultValue(Set())

        // Finally schedule another actor again
        scheduleNext()
        state

    override def scheduleNext(): Unit =
        // the work queue can be empty if all actors are suspended
        // if true, the execution of the program should stop because no progress can be made any further (closed-world)
        if work.isEmpty then state = Stop
        else super.scheduleNext()

    override def next(st: State, version: Version): Either[Value, State] = st match
        // End of the execution of an actor (without a new become)
        case Kont(_, ActC(_)) =>
            scheduleNext()
            Right(state)
        // A stop state stops the program directly, and returns nil
        case Stop =>
            // Check whether there are any unresolved futures
            if createdFutures.values.flatten.size != 0 then throw IncorrectTerminationException(createdFutures)
            Left(Value.Nil)
        // Threads are not allowed in the actor language
        case Kont(v, TrdC(tid)) => throw new Exception("Threads are not supported in AScheme")
        // EndC does not mean the end of the program, since some actors might not have terminated.
        // Only do this if the mailbox is empty or all the actors have terminated
        case Kont(v, EndC()) =>
            resolveTerminationFuture(SimpleActorId(0), v)
            scheduleNext()
            Right(state)
        case _ => super.next(st, version)

    //
    // Eval/Apply
    //

    override def apply(v: Value, cc: Continuation): State = cc match
        //
        // Become/create expression
        //

        // base case, the become/create expression has no arguments, change the behavior or create the actor
        case BecoCreaC(List(), env, isBecome, idn, cc) =>
            if isBecome then become(ensureBehavior(v), List(), cc) else Kont(spawn(ensureBehavior(v), List(), idn), cc)
        // the become/create has arguments that first need to be evaluated
        case BecoCreaC(argument :: arguments, env, isBecome, idn, cc) =>
            Step(argument, env, BecoCreaArgsC(arguments, ensureBehavior(v), List(), env, isBecome, idn, cc))
        // base case, no more arguments to evaluate, change the behavior or create the actor
        case BecoCreaArgsC(List(), beh, vlus, env, isBecome, idn, cc) =>
            val ags = (v :: vlus).reverse
            if isBecome then become(beh, ags, cc) else Kont(spawn(beh, ags, idn), cc)
        // more arguments to evaluate
        case BecoCreaArgsC(argument :: arguments, beh, vlus, env, isBecome, idn, cc) =>
            Step(argument, env, BecoCreaArgsC(arguments, beh, v :: vlus, env, isBecome, idn, cc))

        //
        // Send expression
        //

        // base case, send expression has no arguments, send the message
        case SendC(s @ ASchemeSend(_, msgTpy, List(), _), env, cc) =>
            sendMessage(ensureActor(v), msgTpy.name, List(), s.idn, cc)
        // more arguments to evaluate
        case SendC(send @ ASchemeSend(_, msgTpy, argument :: arguments, _), env, cc) =>
            Step(argument, env, SendArgsC(arguments, List(), ensureActor(v), send, env, cc))
        // base case, no more arguments to evaluate, send message
        case SendArgsC(List(), vlus, actorRef, send, env, cc) =>
            sendMessage(actorRef, send.messageType.name, (v :: vlus).reverse, send.idn, cc)
        // more arguments to evaluate
        case SendArgsC(argument :: arguments, vlus, actorRef, send, env, cc) =>
            Step(argument, env, SendArgsC(arguments, (v :: vlus), actorRef, send, env, cc))

        // Await expressions + Futures
        case AwaiC(cc) =>
            awaitFuture(v, cc)

        case WaiTer(env, cc) =>
            waitForActorTermination(v, cc)

        case TerC(id) =>
            log(s"resolving futures waiting for termination of $id with $v")
            resolveTerminationFuture(id, v)

        //
        // Base Scheme
        //
        case _ => super.apply(v, cc)

    override def eval(exp: SchemeExp, env: Env, version: Version, cc: Continuation): State = exp match
        case ASchemeActor(parameters, selection, idn, name) =>
            Kont(inject(Behavior(name, parameters, selection, env)), cc)

        case ASchemeCreate(behavior, arguments, idn) =>
            Step(behavior, env, BecoCreaC(arguments, env, false, exp.idn, cc))

        case ASchemeBecome(behavior, arguments, idn) =>
            Step(behavior, env, BecoCreaC(arguments, env, true, exp.idn, cc))

        case send @ ASchemeSend(actorRef, _, _, _) =>
            Step(actorRef, env, SendC(send, env, cc))

        case selection @ ASchemeSelect(handlers, idn) =>
            waitForSelect(selection, env, cc)

        case ASchemeAwait(future, _) =>
            Step(future, env, AwaiC(cc))

        case SchemeFuncall(SchemeVar(Identifier("wait-for-termination", _)), List(actorRef), _) =>
            Step(actorRef, env, WaiTer(env, cc))

        case SchemeFuncall(SchemeVar(Identifier("terminate", _)), List(e), _) =>
            val id = ensureEmptyStack(cc)
            Step(e, env, TerC(id))

        case SchemeFuncall(SchemeVar(Identifier("terminate", _)), List(), _) =>
            // actor termination, simply schedule next, and make sure that there is nothing on the stack anymore
            val id = ensureEmptyStack(cc)
            log(s"terminating actor with $id")
            // notify futures about the actors termination
            resolveTerminationFuture(id, Value.Nil)
            scheduleNext()
            state

        case _ => super.eval(exp, env, version, cc)

    /** Returns the termination value of the main actor */
    def getReturnValue: Value =
        resolvedFutures(ActorWaitCompleteFuture(SimpleActorId(0)))
