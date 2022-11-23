package maf.modular.scheme.modactor

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.language.scheme.*
import maf.core.Monad.*
import maf.modular.scheme.modf.TEvalM
import maf.core.*
import maf.language.AScheme.ASchemeValues.*
import maf.language.scheme.SchemeExp
import maf.language.scheme.lattices.SchemeLattice
import maf.language.AScheme.ASchemeLattice
import maf.util.Logger
import maf.modular.scheme.modactor.MirrorValues.Mirror
import maf.modular.scheme.modactor.MirrorValues.Envelope

object MirrorValues:
    /**
     * A runtime representation of the mirror. A mirror is described by a name and an actor reference running the mirror.
     *
     * @param name
     *   the name of the mirror
     * @param tid
     *   an actor reference for the actor running the mirror
     */
    case class Mirror[Ref](name: Option[String], tid: Ref)

    /** An envelope represents a message send as a value by combining the receiver and the message itself */
    case class Envelope[Ref, Value](receiver: Ref, message: Message[Value]):
        override def toString: String = s"($receiver, $message)"

/**
 * Adds support for mirror-based reflection.
 *
 * Reflection is mostly transparent with regards to the analysis results:
 *   - Sensitivity of function calls, and message sends is based on the base layer, sensitivity of the meta-layer is not propagated
 *   - The meta-layer can have impact on the global store.
 *   - The meta-layer is run in a higher-precision mode of the analysis, so that precision is not lost as much.
 *
 * The trait defines the semantics of a meta-layer for actors, it can be used as an inner-modf layer in an actor analysis.
 */
trait ASchemeMirrorsSemantics extends ASchemeSemantics:
    import maf.util.LogOps.*

    /**
     * Monad for supporting mirror-based reflection
     *
     * @tparam M
     *   the monad of which a MetaEvalM instance is provided
     * @tparam ActorRef
     *   the type of actor references
     * @tparam V
     *   the type of abstract values
     */
    trait MetaAnalyisM[M[_]] extends ActorAnalysisM[M]:

        /**
         * Installs the given mirror for the given actor in the actor system
         *
         * @param forActor
         *   the actor for which the mirror should be installed
         * @param mirror
         *   the mirror itself
         */
        def installMirror(forActor: Value, mirror: MirrorValues.Mirror[ActorRef], strong: Boolean = false): M[Unit]

        /**
         * Looks up the mirror for the given actor
         *
         * @param actor
         *   the actor reference to use as a key for querying its mirror
         */
        def lookupMirror(actor: ActorRef): M[Option[MirrorValues.Mirror[ActorRef]]]

    implicit override val analysisM: MetaAnalyisM[A]
    import analysisM.*

    protected type MirrorContext

    private def intercept(f: MirrorValues.Mirror[ActorRef] => A[Value])(otherwise: A[Value]): A[Value] =
        selfActor.flatMap(self =>
            lookupMirror(self).flatMap {
                case Some(mirror) => f(mirror)
                case None         => otherwise
            }
        )

    /**
     * Auxilary function that works like evalM.ask but also allocates a mirror context based on the context of the current component
     *
     * @param actorRef
     *   the receiver of the message
     * @param ags
     *   the list of arguments that should be containing within the message
     * @param idn
     *   the location of the send, can be used for deciding on the mirror context
     */
    private def mirrorAsk(actorRef: ActorRef, tag: String, ags: List[Value], sender: ActorRef, idn: Identity): A[Value] =
        mkMessage(tag, ags) >>= { m => ask(actorRef, m) }

    /** Checks whether the given actor represents a mirror */
    private def mirrorFlagSet(actor: Actor): Boolean = actor.tid match
        case ActorAnalysisComponent(enclosingActor, _, _) => mirrorFlagSet(Actor(None, enclosingActor))
        case maf.modular.scheme.modactor.Actor(beh, _, _) => beh.isMirror

    private def interceptCreate(beh: Value, ags: List[Value], idn: Identity): A[Value] = intercept { mirror =>
        // TODO: actually put the arguments in a scheme list. Figure out a precise allocation scheme for this.
        // TODO: add a mirror parameter here, so that the actor is created with the appropriate mirror
        selfActor.flatMap(self => mirrorAsk(mirror.tid, "create", beh :: ags, self, idn))
    } /* otherwise */ {
        // base behavior
        create(beh, ags, idn)
            .map(actor =>
                if mirrorFlagSet(actor) then lattice.mirrors(Mirror(actor.name, actor))
                else lattice.actor(actor)
            )
    }

    def reifyMessage(m: Msg): Message[Value]
    def abstractMessage(m: Message[Value]): Msg

    private def applyThunk(lam: Value, idn: Identity, ags: List[Value]): A[Value] =
        mjoin(
          lattice.getClosures(lam).map(clo => withEnvM(_ => bindArgs(clo._1.args, ags)(clo._2)) { Monad.sequence(clo._1.body.map(eval)).map(_.last) })
        )

    private def reifyHandler(prs: List[Identifier], bdy: List[SchemeExp], lexEnv: Environment[Address]): Value =
        lattice.closure((SchemeLambda(None, prs, bdy, None, bdy.head.idn), lexEnv))

    override def eval(exp: SchemeExp): A[Value] = exp match
        case ASchemeCreate(beh, ags, idn) =>
            // we intercept actor creation and forward it to the meta layer if necessary
            for
                evaluatedBeh <- eval(beh)
                evaluatedAgs <- Monad.sequence(ags.map(eval))
                result <- interceptCreate(evaluatedBeh, evaluatedAgs, idn)
            yield result
        case ASchemeSend(actorRef, Identifier(tag, _), ags, idn) =>
            for
                evaluatedActor <- eval(actorRef)
                evaluatedAgs <- Monad.sequence(ags.map(eval))
                self <- selfActor
                message <- mkMessage(tag, evaluatedAgs)
                envelope = lattice.envelope(Envelope(self, reifyMessage(message)))
                result <- intercept { mirror =>
                    mirrorAsk(mirror.tid, "send", List(envelope /* , TODO: add the source location of the send here */ ), self, idn)
                } /* otherwise */ {
                    sendMessage(evaluatedActor, tag, evaluatedAgs).map(_ => lattice.nil)
                }
            yield result

        case ASchemeSelect(handlers, idn) =>
            // A message receive in the meta protocol works as follows:
            // If a mirror is installed, a message "receive" is sent using a reification of a message.
            // the receive should respond with a lambda that is supposed to execute the selected behavior when applied.
            // This lambda is applied in the same context as the sending actor, therefore not in the mirror.
            for
                // select the correct handler first
                msg <- receive
                // see if there is an appropriate handler
                tag = getMessageTag(msg)
                args = getMessageArguments(msg)
                handler = handlers.get(tag)
                _ <-
                    if handler.isEmpty then
                        // TODO: actually register an eror
                        mbottom
                    else unit(())
                (pars, bdy) = handler.get
                // reify the message in an abstract value
                reifiedMessage = lattice.message(Message(getMessageTag(msg), getMessageArguments(msg)))
                // send the intercepted message to the mirror if necessary
                result <- intercept { mirror =>
                    for
                        self <- selfActor
                        env <- getEnv
                        // TODO: mirror protocol changed: should reify the behavior not the handler
                        // a primitive called "lookup-handler" should be able to get the appropriate handler
                        // for the given message.
                        lam <- mirrorAsk(mirror.tid, "receive", List(reifiedMessage, reifyHandler(pars, bdy, env)), self, idn)
                        result <- applyThunk(lam, idn, args)
                    yield result
                } /* otherwise */ {
                    log(s"base receive $handler $tag $args")
                    withEnvM(bindArgs(pars, args)) { Monad.sequence(bdy.map(eval)).map(_.last) }
                }
            yield result

        // (create-with-mirror mirrorRef behavior)
        case SchemeFuncall(SchemeVar(Identifier("create-with-mirror", _)), mirrorRef :: behavior :: ags, idn) =>
            // TODO: in the concrete actor implementation we have that the create-with-mirror call
            // is also intercepted. We should do that here as well.
            for
                evaluatedMirrorRef <- eval(mirrorRef)
                evaluatedBehavior <- eval(behavior)
                evaluatedAgs <- ags.mapM(eval)
                actor <- create(evaluatedBehavior, evaluatedAgs, idn).map(lattice.actor)
                _ <- nondets(lattice.getMirrors(evaluatedMirrorRef).map(mirror => installMirror(actor, mirror, strong = true)))
                _ = log(s"+++ intra mirror installed on $actor")
            yield actor

        case SchemeFuncall(SchemeVar(Identifier("base/send-envelope", _)), List(envelopeExpression), _) =>
            for
                evaluatedEnvelope <- eval(envelopeExpression)
                _ = log(s"+++ base/send-envelope $evaluatedEnvelope")
                // get the envelopes from the abstract domain
                envelope <- nondets(lattice.getEnvelopes(evaluatedEnvelope).map(unit))
                // get the receiver, and message from the envelope
                receiver = envelope.receiver
                message = envelope.message
                // send the message to the specified receiver
                result <- send(receiver, abstractMessage(message)).map(_ => lattice.nil)
            yield result

        // For testing purposes
        case SchemeFuncall(SchemeVar(Identifier("error", _)), List(message), _) =>
            for
                m <- eval(message)
                _ = log(s"+++ intra error $m")
                result <- mbottom[Value]
            yield result

        case _ => super.eval(exp)
