package maf.language.AScheme.interpreter

import maf.language.scheme.interpreter.*
import maf.core.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues.*
import maf.language.change.CodeVersion.{New, Old, Version}
import maf.language.AScheme.ASchemeValues.*
import scala.collection.immutable.Queue
import maf.language.ContractScheme.ContractValues
import maf.language.AScheme.ASchemeValues

class CPSASchemeInterpreter(
    cb: (Identity, Value) => Unit = (_, _) => (),
    io: IO = new EmptyIO(),
    stack: Boolean = false,
    cbA: ASchemeInterpreterCallback = ASchemeInterpreterCallback.EmptyCallback)
    extends CPSSchemeInterpreter(cb, io, stack):

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

    //
    // Interpreter structures
    //

    private type M = Message[Value]
    private type AID = SimpleActorId

    /** The set of mailboxes for each actor id */
    private var mailboxes: Map[AID, Queue[M]] = Map().withDefaultValue(Queue())

    /** A set of suspended states for each actor. Usually, actors are only suspended when it is waiting for messages */
    private var suspended: Map[AID, State] = Map()

    /** The map from actor ids to concrete actor references */
    private var actors: Map[AID, Actor] = Map()

    /** Counter to keep track of the last used actor id */
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
        println(s"Spawning actor $beh with args $ags")
        val id = allocActorId()
        val actorRef = ConcreteActor(beh.name, id, beh, idn)
        // maintain the bookkeeping
        actors = actors + (id -> actorRef)
        cbA.spawn(actorRef)

        // extend the environment with bindings for the actor arguments
        val extendedEnv = extendEnv(Identifier("self", Identity.none) :: beh.prs, actorRef :: ags, beh.lexEnv)

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
                println("suspending actor")
                // suspend if no messages for this actor
                suspended = suspended + (id -> (Kont(Value.Nil, Wait(selection, env, id))))
                scheduleNext()
                state
            else
                println("receiving message from mailbox")
                // continue if there is a message for this actor
                val (ms, mb) = mailboxes(id).dequeue
                mailboxes = mailboxes + (id -> mb)
                handleMessage(ms, selection, env, cc)
        case _ => throw new Exception(s"`select` must be the last expression at ${selection.idn}")

    private def popSuspended(id: AID): Option[State] =
        suspended.get(id) match
            case s @ Some(st) =>
                println(s"waking $st from $id")
                suspended = suspended - id
                s
            case None => None

    /** Handle the given message using the given select statement */
    private def handleMessage(ms: M, selection: ASchemeSelect, env: Env, cc: Continuation): State =
        selection.handlers.get(ms.tag) match
            case Some((prs, first :: rest)) =>
                val extendedEnv = extendEnv(prs, ms.vlus, env)
                val nextCc = if rest.nonEmpty then BegC(rest, env, cc) else cc
                Step(first, extendedEnv, nextCc)
            case Some((prs, List())) =>
                Kont(Value.Nil, cc)
            case None => throw new Exception(s"no suitable handler for ${ms.tag}")

    /** Asynchronously sends a message to the given actor's mailbox */
    private def sendMessage(actorRef: Actor, tag: String, ags: List[Value], idn: Identity, cc: Continuation): State =
        val id = asSimpleActorId(actorRef.tid)
        val ms = Message(tag, ags)
        cbA.sendMessage(actors(id), ms, idn)
        println(s"sending message $ms to $actorRef")
        popSuspended(id) match
            case Some(Kont(_, Wait(selection, env, id))) =>
                // schedule the delivery of the message if the actor is already waiting
                work = work.enqueue(handleMessage(ms, selection, env, ActC(id)))
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
        case _        => throw new Exception(s"unexpected behavior $cc")

    /** Become a different behavior */
    private def become(beh: Behavior, ags: List[Value], cc: Continuation): State =
        val id = ensureEmptyStack(cc)
        // bookkeeping
        cbA.become(actors(id), beh)
        // enqueue the body of the behavior
        val extendedEnv = extendEnv(Identifier("self", Identity.none) :: beh.prs, actors(id) :: ags, beh.lexEnv)
        work = work.enqueue(Step(beh.bdy, extendedEnv, cc))

        // fairness: after an actor has performed a single turn, it should yield control to another actor
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
        case Stop => Left(Value.Nil)
        // Threads are not allowed in the actor language
        case Kont(v, TrdC(tid)) => throw new Exception("Threads are not supported in AScheme")
        // EndC does not mean the end of the program, since some actors might not have terminated.
        // Only do this if the mailbox is empty or all the actors have terminated
        case Kont(_, EndC()) =>
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

        case SchemeFuncall(SchemeVar(Identifier("terminate", _)), List(), _) =>
            // actor termination, simply schedule next, and make sure that there is nothing on the stack anymore
            val id = ensureEmptyStack(cc)
            println(s"terminating actor with $id")
            scheduleNext()
            state

        case _ => super.eval(exp, env, version, cc)
