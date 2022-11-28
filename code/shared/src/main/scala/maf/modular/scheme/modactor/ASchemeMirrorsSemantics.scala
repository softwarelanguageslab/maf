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

/** An error produced by using base/fail */
case class MirrorError[V](message: V, idn: Identity) extends maf.core.Error

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

        /** Fail in the base interpreter */
        def baseFail(error: Value): M[Unit]

        /** Predicate that returns true if the resposne from the mirror was an error */
        def isFail(vlu: Value): M[Boolean]

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

    /**
     * Intercept the `create` primitive of the actor.
     *
     * @param beh
     *   the behavior that is being created
     * @param ags
     *   a list of abstract values to be used as constructor arguments for the behavior
     * @param mirror
     *   an abstract representation of a mirror or an abstract representation of #f indicating that there is no mirror
     * @param exs
     *   a list of expressions corresponding to the constructor arguments for the behavior
     * @param idn
     *   the identity of the create expression.
     */
    private def interceptCreate(beh: Value, ags: List[Value], m: Value, exs: List[SchemeExp], idn: Identity): A[Value] = intercept { mirror =>
        for
            arguments <- allocLst(exs.zip(ags))
            self <- selfActor
            result <- mirrorAsk(mirror.tid, "create", List(m, beh, arguments), self, idn)
        yield result
    } /* otherwise */ {
        // base behavior
        baseCreate(beh, m, ags, idn)
    }

    private def baseCreate(beh: Value, evaluatedMirrorRef: Value, ags: List[Value], idn: Identity): A[Value] =
        for
            actor <- create(beh, ags, idn)
            actorAbs =
                if mirrorFlagSet(actor) then lattice.mirrors(Mirror(actor.name, actor))
                else lattice.actor(actor)
            // install the mirror if necessary
            // TODO: if the mirror is a function, then apply it first before installing the mirror
            _ <- nondets(lattice.getMirrors(evaluatedMirrorRef).map(mirror => installMirror(actorAbs, mirror, strong = true)))
        yield actorAbs

    /**
     * Reify the given message to a value that can be used by the meta-layer.
     *
     * @param m
     *   the message to reify as a value
     * @param exps
     *   the expressions corresponding to the payload of the message
     */
    def reifyMessage(m: Msg, exps: List[SchemeExp]): Message[Value]

    /** Injects a concrete message into the abstract domain */
    def abstractMessage(m: Message[Value]): Msg

    private def applyThunk(lam: Value, idn: Identity, ags: List[Value]): A[Value] =
        mjoin(
          lattice.getClosures(lam).map(clo => withEnvM(_ => bindArgs(clo._1.args, ags)(clo._2)) { Monad.sequence(clo._1.body.map(eval)).map(_.last) })
        )

    private def reifyHandler(prs: List[Identifier], bdy: List[SchemeExp], lexEnv: Environment[Address]): Value =
        lattice.closure((SchemeLambda(None, prs, bdy, None, bdy.head.idn), lexEnv))

    /** Convert a Scheme list to a Scala list */
    protected def asScalaList(vlu: Val): A[List[Val]] = ???

    object MetaPrimitives:
        lazy val primitives = {
            List(
              `base/create`,
              `lookup-handler`
            ) ++ envelope.primitives ++ message.primitives
        }.map(p => p.name -> p).toMap

        lazy val primitiveNames = primitives.keys.toSet

        def isPrimitive(id: String): Boolean = primitiveNames.contains(id)

        trait MetaPrimitive(val name: String):
            def call(args: List[Value], idn: Identity): A[Value]
            def checkArity(args: List[Value], expected: Int): A[Unit] =
                if args.size < expected then mbottom else unit(())

        abstract class Struct[T]:
            def lift(f: T => Value): T => A[Value] =
                f andThen unit
            def get(v: Value): A[T]
            def fields: Map[String, T => A[Value]]
            def primitives: List[MetaPrimitive] =
                fields.map { case (name, f) =>
                    new MetaPrimitive(name) {
                        def call(args: List[Value], idn: Identity): A[Value] = checkArity(args, 1) >>> get(args(0)) >>= f
                    }
                }.toList

        case object `base/create` extends MetaPrimitive("create/c"):
            def call(args: List[Value], idn: Identity): A[Value] =
                for
                    _ <- checkArity(args, 3)
                    arguments <- asScalaList(args(2))
                    result <- baseCreate(args(0), args(1), arguments, idn)
                yield result

        case object `lookup-handler` extends MetaPrimitive("lookup-handler"):
            def call(args: List[Value], idn: Identity): A[Value] =
                for
                    _ <- checkArity(args, 2)
                    beh <- nondets(lattice.getBehs(args(0)).map(unit))
                    tag = args(1)
                    // NOTE: might be a source of imprecision since the argument
                    // of lookup might be overapproximated.
                    handler <- nondets(beh.lookupHandler(tag).map(unit))
                    closure <- withEnv(_ => beh.lexEnv) { eval(handler) }
                yield closure

        case object `base/fail` extends MetaPrimitive("base/fail"):
            def call(args: List[Value], idn: Identity): A[Value] =
                for
                    _ <- checkArity(args, 2)
                    interpreter = args(0)
                    message = args(1)
                    _ <- baseFail(message)
                yield lattice.void

        case object envelope extends Struct[Envelope[Actor, Value]]:
            def get(v: Value) = nondets(lattice.getEnvelopes(v).map(unit))
            val fields = Map(
              "receiver" -> lift(ev => lattice.actor(ev.receiver)),
              "message" -> lift(ev => lattice.message(ev.message))
            )

        case object message extends Struct[Message[Value]]:
            def get(v: Value) = nondets(lattice.getMessages(v).map(unit))
            def fields = Map(
              "tag" -> lift(m => lattice.symbol(m.tag)),
              "arguments" -> { message =>
                  allocLst(message.exs.zip(message.vlus))
              }
            )

    /**
     * Evaluates the given expression to a valid mirror ref.
     *
     * If the returned value is a lambda it is applied with the given actor value, otherwise it is returned directly
     */
    protected def evalMirrorRef(mirrorRef: SchemeExp, actor: Value): A[Value] =
        eval(mirrorRef).flatMap { vlu =>
            // TODO: call the lambda with the actor
            nondets(lattice.getClosures(vlu).map[Value](clo => ???).map(unit) ++ lattice.getActors(vlu).map(lattice.actor andThen unit))
        }

    override def eval(exp: SchemeExp): A[Value] = exp match
        case ASchemeCreate(beh, ags, idn) =>
            // we intercept actor creation and forward it to the meta layer if necessary
            for
                evaluatedBeh <- eval(beh)
                evaluatedAgs <- Monad.sequence(ags.map(eval))
                result <- interceptCreate(evaluatedBeh, evaluatedAgs, lattice.bool(false), ags, idn)
            yield result
        case ASchemeSend(actorRef, Identifier(tag, _), ags, idn) =>
            for
                evaluatedActor <- eval(actorRef)
                evaluatedAgs <- Monad.sequence(ags.map(eval))
                self <- selfActor
                message <- mkMessage(tag, evaluatedAgs)
                envelope = lattice.envelope(Envelope(self, reifyMessage(message, ags)))
                result <- intercept { mirror =>
                    mirrorAsk(mirror.tid, "send", List(envelope /* , TODO: add the source location of the send here */ ), self, idn).flatMap(vlu =>
                        if lattice.isError(vlu) then fail(MirrorError(vlu, idn)).map(_ => lattice.void) else unit(lattice.void)
                    )
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
                evaluatedBehavior <- eval(behavior)
                evaluatedAgs <- ags.mapM(eval)
                actor <- create(evaluatedBehavior, evaluatedAgs, idn).map(lattice.actor)
                evaluatedMirrorRef <- evalMirrorRef(mirrorRef, actor)
                _ <- nondets(lattice.getMirrors(evaluatedMirrorRef).map(mirror => installMirror(actor, mirror, strong = true)))
                _ = log(s"+++ intra mirror installed on $actor")
            yield actor

        case SchemeFuncall(SchemeVar(Identifier("base/send-envelope" | "send-envelope", _)), List(envelopeExpression), _) =>
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
