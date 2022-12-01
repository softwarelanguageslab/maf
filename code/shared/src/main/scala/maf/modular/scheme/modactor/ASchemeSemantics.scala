package maf.modular.scheme.modactor

import maf.modular.scheme.modflocal.*
import maf.modular.scheme.SchemeDomain
import maf.core.*
import maf.language.AScheme.ASchemeValues
import maf.language.AScheme.ASchemeValues.*
import maf.language.scheme.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.AScheme.ASchemeLattice
import maf.util.Logger

/** The address in the store in which a reference to the current actor is stored */
case object SelfAdr extends Address {
    override def printable: Boolean = false
    override def idn: Identity = Identity.none
}

trait ASchemeSemantics extends SchemeSemantics, SchemeModFLocalSensitivity, SchemeDomain:
    import Monad.*
    import MonadJoin.*
    import maf.util.LogOps.*

    given Logger.Logger = Logger.ConsoleLog()

    type ActorRef = Actor
    type Behavior = ASchemeValues.Behavior

    type MessageContext
    def emptyContext: MessageContext

    type Msg

    implicit override lazy val lattice: ASchemeLattice[Val, Adr]

    trait MessageM[M[_]]:
        /**
         * Create a new message
         *
         * @param tag
         *   the tag of the message
         * @param arguments
         *   a list of values representing the arguments of the message
         * @return
         *   the new message wrapped in the analysis monad
         */
        def mkMessage(tpy: String, arguments: List[Value]): M[Msg]

        /** Get the tag of a message */
        def getMessageTag(m: Msg): String

        /** Get the arguments of a message */
        def getMessageArguments(m: Msg): List[Val]

    trait ActorAnalysisM[M[_]] extends AnalysisM[M], MessageM[M]:
        /**
         * Send a message to the given actor
         *
         * @param to
         *   the actor to which the message should be sent
         * @param msg
         *   the message to send
         */
        def sendMessage(to: Val, tag: String, ags: List[Val], context: MessageContext): M[Unit] =
            given Monad[M] = this
            mkMessage(tag, ags).flatMap { msg =>
                mjoin(
                  lattice
                      .getActors(to)
                      .map { actor =>
                          send(actor, msg, context)
                      }
                      .toList
                )
            }

        def send(to: ActorRef, m: Msg, context: MessageContext): M[Unit]
        def send_(context: MessageContext)(to: ActorRef)(m: Msg): M[Unit] = send(to, m, context)

        /** Same as sends but "blocks" until an answer has been received */
        def ask(to: ActorRef, m: Msg, context: MessageContext): M[Value]

        def nondets[X](xs: Iterable[M[X]]): M[X]

        /** Create an actor */
        def create(beh: Value, ags: List[Value], idn: Identity, defer: Boolean = false): M[ActorRef] =
            nondets(lattice.getBehs(beh).map(b => spawnActor(b, ags, idn, defer)))

        /** Become a new behavior */
        def become(beh: Behavior, ags: List[Value], idn: Identity): M[Unit]

        /** Returns a reference to self */
        def selfActor: M[ActorRef]

        /**
         * Receive a message from the mailbox
         *
         * @return
         *   the message wrapped in the analysis monad
         */
        def receive: M[(Msg, MessageContext)]

        /**
         * Spawn a new actor
         *
         * @param defer
         *   true if queuing the actor for analysis should be defered to a later point in time
         * @return
         *   a value representing the actor reference of the just created actor
         */
        def spawnActor(beh: Behavior, ags: List[Value], idn: Identity, defer: Boolean = false): M[ActorRef]

        /** Queue an actor for analysis that has been defered by spawnActor */
        def deferedSpawnActor(actor: ActorRef): M[Unit]

    implicit override val analysisM: ActorAnalysisM[A]
    import analysisM.*

    /** Change the behavior of the current actor to the given behavior */
    private def become(beh: Val, ags: List[Val], idn: Identity): A[Unit] =
        mjoin(lattice.getBehs(beh).map { beh =>
            analysisM.become(beh, ags, idn)
        })

    protected def bindArgs(prs: List[Identifier], vlus: List[Val])(env: Environment[Address]): A[Environment[Address]] =
        prs.zip(vlus).foldLeftM(env) { case (env, (par, vlu)) =>
            allocVar(par).flatMap(addr => extendSto(addr, vlu) >>> unit(env.extend(par.name, addr)))
        }

    override protected def evalVariable(id: Identifier): A[Val] = id.name match
        case "self" | "a/self" => selfActor.map(lattice.actor)
        case _                 => super.evalVariable(id)

    override def eval(exp: SchemeExp): A[Val] =
        exp match
            // An actor expression evaluates to a behavior
            case ASchemeActor(parameters, selection, _, name, isMirror) =>
                for env <- getEnv
                yield lattice.beh(Behavior(name, parameters, selection, env, isMirror))

            // Evaluating a create expression spawns a new actor and returns a reference to it
            case ASchemeCreate(beh, ags, idn) =>
                for
                    env <- getEnv
                    evaluatedBeh <- nontail(eval(beh))
                    evaluatedAgs <- evalAll(ags)
                    actorRef <- create(evaluatedBeh, evaluatedAgs, idn)
                yield lattice.actor(actorRef)

            // Sending a message is atomic (asynchronous) and results in nil
            case ASchemeSend(actorRef, messageTpy, ags, idn) =>
                for
                    evaluatedActorRef <- nontail(eval(actorRef))
                    evaluatedAgs <- evalAll(ags)
                    _ = { log(s"++ intra - actor/send $evaluatedActorRef $messageTpy $evaluatedAgs") }
                    // TODO: the context should be inherited from the current context.
                    // perhaps add a new function to the monad to create a new context based on the
                    // previous one.
                    _ <- sendMessage(evaluatedActorRef, messageTpy.name, evaluatedAgs, emptyContext)
                yield lattice.nil

            // Change the behavior of the current actor, and return nil
            case ASchemeBecome(beh, ags, idn) =>
                for
                    evaluatedBeh <- nontail(eval(beh))
                    evaluatedAgs <- evalAll(ags)
                    _ <- become(evaluatedBeh, evaluatedAgs, idn)
                    // we stop the analysis here because the actor is in a suspended state,
                    // waiting for new messages defined by the behavior.
                    //
                    // The intra-actor analysis will restart the analysis when appropriate
                    res <- mbottom[Value]
                yield res

            // Stay in the same behavior
            case SchemeFuncall(SchemeVar(Identifier("same-behavior", _)), List(), _) =>
                // since no new behavior is produced we can simply return bottom
                mbottom[Value]

            case SchemeFuncall(SchemeVar(Identifier("same-behavior", _)), _, idn) =>
                throw new Exception(s"Invalid syntax: same-behavior at $idn")

            // Receive a message from the mailbox
            case ASchemeSelect(handlers, idn) =>
                for
                    // select a message from the mailbox
                    msgWithContext <- receive
                    (msg, context) = msgWithContext
                    _ = { log(s"actor/recv $msg") }
                    // see if there is an applicable handler
                    tag = getMessageTag(msg)
                    args = getMessageArguments(msg)
                    handler = handlers.get(tag)
                    result <-
                        if handler.isEmpty then mbottom
                        else
                            val (pars, bdy) = handler.get
                            withEnvM(bindArgs(pars, args)) {
                                evalSequence(bdy)
                            } >>= trace(s"actor/recv $msg result")
                yield lattice.nil

            case SchemeFuncall(SchemeVar(Identifier("terminate", _)), _, _) =>
                // actor termination
                mbottom

            case _ => super.eval(exp)
