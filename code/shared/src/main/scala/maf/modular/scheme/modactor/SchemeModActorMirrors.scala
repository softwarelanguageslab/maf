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

/** Monad for supporting actor operaton */
trait ActorEvalM[M[_], Context, ActorRef, V] extends TEvalM[M]:
    /**
     * Send a message to the given actor
     *
     * @param to
     *   the actor to which the message should be sent
     * @param msg
     *   the message to send
     */
    def sendMessage(to: V, tag: String, ags: List[V]): M[V]

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
     *   the value that was the result of the ask pattern, if a suitable lattice is available, "bottom" can be used to signal that the value is not
     *   yet available, or mzero can be returned to stop the analysis until the value becomes available.
     */
    def ask(
        to: ActorRef,
        tag: String,
        ags: List[V],
        sender: ActorRef,
        context: Context,
      ): M[V]

    /** Create an actor */
    def create(beh: Behavior, ags: List[V], idn: Identity): M[ActorRef]

    /** Returns a reference to self */
    def selfActor: M[ActorRef]

    /**
     * Receive a message from the mailbox message and select a handler
     *
     * @return
     *   a monad that returns a tuple of: handler paraemeters, handler body, message arguments and a reified representation of a message
     */
    def receive(handlers: Map[String, (List[Identifier], List[SchemeExp])]): M[(List[Identifier], List[SchemeExp], List[V], V)]

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
trait MetaEvalM[M[_], Context, ActorRef, V] extends ActorEvalM[M, Context, ActorRef, V]:

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
trait SchemeModActorMirrors[ActorRef <: AID, Beh] extends BigStepModFSemanticsT:
    override def intraAnalysis(component: Component): IntraMirrorAnalysis
    implicit override val evalM: MetaEvalM[EvalM, MirrorContext, ActorRef, Value]
    implicit override lazy val lattice: ASchemeLattice[Value, Address]

    protected type MirrorContext
    protected def allocMirrorCtx(baseContext: Option[ComponentContext], sendIdentity: Identity): MirrorContext

    trait IntraMirrorAnalysis extends BigStepModFIntraT:
        private def intercept(f: MirrorValues.Mirror[ActorRef] => EvalM[Value])(otherwise: EvalM[Value]): EvalM[Value] =
            evalM.selfActor.flatMap(self =>
                evalM.lookupMirror(self).flatMap {
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
        private def mirrorAsk(actorRef: ActorRef, tag: String, ags: List[Value], sender: ActorRef, idn: Identity): EvalM[Value] =
            val ctx = allocMirrorCtx(context(component), idn)
            evalM.ask(actorRef, tag, ags, sender, ctx)

        private def interceptCreate(beh: Value, ags: List[Value], idn: Identity): EvalM[Value] = intercept { mirror =>
            // TODO: actually put the arguments in a scheme list. Figure out a precise allocation scheme for this.
            evalM.selfActor.flatMap(self => mirrorAsk(mirror.tid, "create", beh :: ags, self, idn))
        } /* otherwise */ {
            // base behavior
            evalM.merge(
              lattice
                  .getBehs(beh)
                  .map { beh =>
                      evalM.create(beh, ags, idn).map(aid => lattice.actor(Actor(beh.name, aid)))
                  }
            )
        }

        private def applyThunk(lam: Value, idn: Identity): EvalM[Value] =
            evalM.merge(lattice.getClosures(lam).map(clo => applyClosuresM(lam, List(), idn.pos)))

        private def reifyHandler(prs: List[Identifier], bdy: List[SchemeExp], lexEnv: Environment[Address]): Value =
            lattice.closure((SchemeLambda(None, prs, bdy, None, bdy.head.idn), lexEnv))

        override protected def eval(exp: SchemeExp): EvalM[Value] = exp match
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
                    self <- evalM.selfActor
                    result <- intercept { mirror => mirrorAsk(mirror.tid, "send", evaluatedActor :: evaluatedAgs, self, idn) } /* otherwise */ {
                        evalM.sendMessage(evaluatedActor, tag, evaluatedAgs)
                    }
                yield result

            case ASchemeSelect(handlers, idn) =>
                // A message receive in the meta protocol works as follows:
                // If a mirror is installed, a message "receive" is sent using a reification of a message.
                // the receive should respond with a lambda that is supposed to execute the selected behavior when applied.
                // This lambda is applied in the same context as the sending actor, therefore not in the mirror.
                for
                    // select the correct handler first
                    receiveRes <- evalM.receive(handlers)
                    (pars, bdy, ags, msg) = receiveRes
                    result <- intercept { mirror =>
                        for
                            self <- evalM.selfActor
                            env <- evalM.getEnv
                            lam <- mirrorAsk(mirror.tid, "receive", List(msg, reifyHandler(pars, bdy, env)), self, idn)
                            result <- applyThunk(lam, idn)
                        yield result
                    } /* otherwise */ { evalM.withEnvM(env => bind(pars.zip(ags).toList, env)) { Monad.sequence(bdy.map(eval)).map(_.last) } }
                yield result

            case _ => super.eval(exp)
