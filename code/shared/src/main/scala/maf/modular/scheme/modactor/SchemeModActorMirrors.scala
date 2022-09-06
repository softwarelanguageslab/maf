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

object MirrorValues:
    /**
     * A runtime representation of the mirror. A mirror is described by a name and an actor reference running the mirror.
     *
     * @param name
     *   the name of the mirror
     * @param tid
     *   an actor reference for the actor running the mirror
     */
    case class Mirror[Ref](name: String, tid: Ref)

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
trait SchemeModActorMirrors[Beh] extends ASchemeSemantics:
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
        def installMirror(forActor: ActorRef, mirror: MirrorValues.Mirror[ActorRef]): M[Unit]

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
        mkMessage(tag, lattice.actor(sender) :: ags) >>= { m => ask(actorRef, m) }

    private def interceptCreate(beh: Value, ags: List[Value], idn: Identity): A[Value] = intercept { mirror =>
        // TODO: actually put the arguments in a scheme list. Figure out a precise allocation scheme for this.
        selfActor.flatMap(self => mirrorAsk(mirror.tid, "create", beh :: ags, self, idn))
    } /* otherwise */ {
        // base behavior
        create(beh, ags, idn).map(lattice.actor)
    }

    private def applyThunk(lam: Value, idn: Identity): A[Value] =
        mjoin(lattice.getClosures(lam).map(clo => applyClosure(SchemeFuncall(clo._1, List(), Identity.none), clo._1, List(), List())))

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
                result <- intercept { mirror => mirrorAsk(mirror.tid, "send", evaluatedActor :: evaluatedAgs, self, idn) } /* otherwise */ {
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
                (pars, bdy) = handler.get
                // reify the message in an abstract value
                reifiedMessage = lattice.message(Message(getMessageTag(msg), getMessageArguments(msg)))
                // send the intercepted message to the mirror if necessary
                result <- intercept { mirror =>
                    for
                        self <- selfActor
                        env <- getEnv
                        lam <- mirrorAsk(mirror.tid, "receive", List(reifiedMessage, reifyHandler(pars, bdy, env)), self, idn)
                        result <- applyThunk(lam, idn)
                    yield result
                } /* otherwise */ { withEnvM(bindArgs(pars, args)) { Monad.sequence(bdy.map(eval)).map(_.last) } }
            yield result

        case _ => super.eval(exp)
