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

trait ASchemeSemantics extends SchemeSemantics, SchemeModFLocalSensitivity, SchemeDomain:
    import Monad.*
    import MonadJoin.*
    import maf.util.LogOps.*

    given Logger.Logger = Logger.DisabledLog()

    type ActorRef = Actor
    type Behavior = ASchemeValues.Behavior
    type Message

    implicit override lazy val lattice: ASchemeLattice[Val, Adr]

    trait ActorAnalysisM[M[_]] extends AnalysisM[M]:
        /**
         * Send a message to the given actor
         *
         * @param to
         *   the actor to which the message should be sent
         * @param msg
         *   the message to send
         */
        def sendMessage(to: Val, tag: String, ags: List[Val]): M[Val] =
            given Monad[M] = this
            mkMessage(tag, ags).flatMap { msg =>
                merge(
                  lattice
                      .getActors(to)
                      .map { actor =>
                          send(actor, msg)
                      }
                      .toList
                )
            }

        def send(to: ActorRef, m: Message): M[Val]

        /**
         * Sends a message using the "ask" pattern to the given actor
         *
         * @param to
         *   the actor to receive the message
         * @param msg
         *   the message itself
         * @param sender
         *   the sender of the message
         * @param context
         *   the analysis context in which the ask messages should be sent
         * @param idn
         *   a location associated with the send
         * @return
         *   the value that was the result of the ask pattern, if a suitable lattice is available, "bottom" can be used to signal that the value is
         *   not yet available, or mzero can be returned to stop the analysis until the value becomes available.
         */
        def ask(
            to: ActorRef,
            tag: String,
            ags: List[Value],
            sender: ActorRef,
            context: Ctx,
          ): M[Value]

        /** Create an actor */
        def create(beh: Behavior, ags: List[Value], idn: Identity): M[ActorRef]

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
        def receive: M[Message]

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
        def mkMessage(tpy: String, arguments: List[Value]): M[Message]

        /** Get the tag of a message */
        def getMessageTag(m: Message): String

        /** Get the arguments of a message */
        def getMessageArguments(m: Message): List[Val]

        /**
         * Spawn a new actor
         *
         * @return
         *   a value representing the actor reference of the just created actor
         */
        def spawnActor(beh: Value, ags: List[Value], idn: Identity): M[Value]

    implicit override val analysisM: ActorAnalysisM[A]
    import analysisM.*

    /** Change the behavior of the current actor to the given behavior */
    private def become(beh: Val, ags: List[Val], idn: Identity): A[Unit] =
        merge(lattice.getBehs(beh).map { beh =>
            analysisM.become(beh, ags, idn)
        })

    protected def bindArgs(prs: List[Identifier], vlus: List[Val])(env: Environment[Address]): A[Environment[Address]] =
        prs.zip(vlus).foldLeftM(env) { case (env, (par, vlu)) =>
            allocVar(par).flatMap(addr => extendSto(addr, vlu) >>> unit(env.extend(par.name, addr)))
        }

    override def eval(exp: SchemeExp): A[Val] = exp match
        // An actor expression evaluates to a behavior
        case ASchemeActor(parameters, selection, _, name) =>
            for env <- getEnv
            yield lattice.beh(Behavior(name, parameters, selection, env))

        // Evaluating a create expression spawns a new actor and returns a reference to it
        case ASchemeCreate(beh, ags, idn) =>
            for
                evaluatedBeh <- eval(beh)
                env <- getEnv
                evaluatedAgs <- ags.mapM(eval)
                actorRef <- spawnActor(evaluatedBeh, evaluatedAgs, idn)
            yield actorRef

        // Sending a message is atomic (asynchronous) and results in nil
        case ASchemeSend(actorRef, messageTpy, ags, idn) =>
            for
                evaluatedActorRef <- eval(actorRef)
                evaluatedAgs <- ags.mapM(eval)
                _ <- sendMessage(evaluatedActorRef, messageTpy.name, evaluatedAgs)
            yield lattice.nil

        // Change the behavior of the current actor, and return nil
        case ASchemeBecome(beh, ags, idn) =>
            for
                evaluatedBeh <- eval(beh)
                evaluatedAgs <- ags.mapM(eval)
                _ <- become(evaluatedBeh, evaluatedAgs, idn)
                // we stop the analysis here because the actor is in a suspended state,
                // waiting for new messages defined by the behavior.
                //
                // The intra-actor analysis will restart the analysis when appropriate
                res <- mbottom[Value]
            yield res

        // Receive a message from the mailbox
        case ASchemeSelect(handlers, idn) =>
            for
                // select a message from the mailbox
                msg <- receive
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
                            Monad.sequence(bdy.map(eval))
                        } >>= trace(s"actor/recv $msg result")
            yield lattice.nil

        case SchemeFuncall(Identifier("terminate", _), _, _) =>
            // actor termination
            mbottom

        case _ => super.eval(exp)
