package maf.modular.scheme.modactor

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.language.scheme.*
import maf.core.Monad.*
import maf.core.MonadJoin.*
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
    case class Envelope[Ref, Value](receiver: Ref, message: AbstractMessage[Value]):
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

    abstract class MirrorMessageContext
    case class SendSiteContext(senderSite: Identity) extends MirrorMessageContext
    case object NoContext extends MirrorMessageContext

    def emptyContext: MessageContext = NoContext

    type MessageContext = MirrorMessageContext

    /** Inject the message context as part of the analysis context */
    def messageCtx(mCtx: MessageContext)(ctx: Ctx): Ctx

    type Msg = AbstractMessage[Value]

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

        /** Returns the current behavior (if any) */
        def currentBehavior: M[Behavior]

    implicit override val analysisM: MetaAnalyisM[A]
    import analysisM.*

    protected type MirrorContext

    private def intercept(f: MirrorValues.Mirror[ActorRef] => A[Value])(otherwise: => A[Value]): A[Value] =
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
        // TODO: change emptyContext to something that keeps track of thez send site
        Message.meta(tag, ags, idn) >>= { m => ask(actorRef, m, SendSiteContext(idn)) }

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
            _ = log(s"+++ meta/create got arguments $arguments")
            self <- selfActor
            _ = log(s"+++ meta/create got self $self")
            result <- mirrorAsk(mirror.tid, "create", List(m, beh, arguments), self, idn)
            _ = log(s"+++ meta/create got result from mirror $result")
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
            _ <-
                if lattice.isFalse(evaluatedMirrorRef) then unit(())
                else nondets(lattice.getMirrors(evaluatedMirrorRef).map(mirror => installMirror(actorAbs, mirror, strong = true)))
        yield actorAbs

    /**
     * Reify the given message to a value that can be used by the meta-layer.
     *
     * @param m
     *   the message to reify as a value
     * @param exps
     *   the expressions corresponding to the payload of the message
     */
    def reifyMessage(m: Msg, exps: List[SchemeExp]): AbstractMessage[Value]

    private def applyThunk(lam: Value, idn: Identity, ags: List[Value]): A[Value] =
        mjoin(
          lattice.getClosures(lam).map(clo => withEnvM(_ => bindArgs(clo._1.args, ags)(clo._2)) { Monad.sequence(clo._1.body.map(eval)).map(_.last) })
        )

    private def applyThunk(lam: Value, idn: Identity, ags: Value): A[Value] =
        mjoin(
          lattice
              .getClosures(lam)
              .map(clo =>
                  if clo._1.varArgId.isDefined then throw new Exception("Variable number of arguments is not supported")
                  val siz = clo._1.args.size
                  ags.take(primitives)(siz)
                      .flatMap(ags => withEnvM(_ => bindArgs(clo._1.args, ags)(clo._2)) { Monad.sequence(clo._1.body.map(eval)).map(_.last) })
              )
        )

    private def reifyHandler(prs: List[Identifier], bdy: List[SchemeExp], lexEnv: Environment[Address]): Value =
        lattice.closure((SchemeLambda(None, prs, bdy, None, bdy.head.idn), lexEnv))

    object MetaPrimitives:
        // TODO: actually add these primitives to the initial environment,
        // and make them implement the corect interface
        lazy val primitives = {
            List(
              `base/create`,
              `lookup-handler`,
              `base/fail`
            ) ++ envelope.primitives ++ message.primitives
        }.map(p => p.name -> p).toMap

        lazy val primitiveNames = primitives.keys.toSet

        def isPrimitive(id: String): Boolean = primitiveNames.contains(id)

        trait MetaPrimitive(val name: String):
            def call(args: List[Value], idn: Identity): A[Value]
            def checkArity(args: List[Value], expected: Int, strict: Boolean = true): A[Unit] =
                if args.size < expected || (strict && args.size != expected) then mbottom else unit(())

        abstract class Struct[T]:
            /** The name of the struct */
            val structName: String

            /** Lift a function in the monadic context */
            def lift(f: T => Value): T => A[Value] =
                f andThen unit

            /** Get the internal representation of the struct from a value */
            def get(v: Value): A[T]

            /** All the fields that should be accessible inside the struct and a computation to access them */
            def fields: Map[String, T => A[Value]]

            /** The number of fields in the struct, the same as the number of keys in the fields map by default */
            def fieldSize: Int = fields.keys.size

            /** Construct an instance of the struct */
            def mk(constructorArguments: List[Value], idn: Identity): A[Value]

            /** A list of all primitives generated by the struct */
            def primitives: List[MetaPrimitive] =
                val accessors = fields.map { case (name, f) =>
                    new MetaPrimitive(s"$structName-$name") {
                        def call(args: List[Value], idn: Identity): A[Value] = checkArity(args, 1) >>> get(args(0)) >>= f
                    }
                }.toList
                val constructor = new MetaPrimitive(s"$structName") {
                    def call(args: List[Value], idn: Identity): A[Value] =
                        for
                            _ <- checkArity(args, fieldSize)
                            s <- mk(args, idn)
                        yield s
                }

                constructor :: accessors

        case object `base/create` extends MetaPrimitive("base/create"):
            def call(args: List[Value], idn: Identity): A[Value] =
                for
                    _ <- checkArity(args, 2, strict = false)
                    arguments = args.drop(2)
                    result <- baseCreate(args(1), args(0), arguments, idn)
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
                    interpreter <- nondets(lattice.getActors(args(0)).map(unit))
                    message = args(1)
                    mm <- Message.meta("answer", List(lattice.error(message)), idn)
                    // TODO: change emptyContext to actual context
                    _ <- send(interpreter, mm, emptyContext)
                yield lattice.void

        case object envelope extends Struct[Envelope[Actor, Value]]:
            val structName: String = "envelope"
            def mk(constructorArguments: List[Value], idn: Identity): A[Value] =
                for
                    act <- nondets(lattice.getActors(constructorArguments(0)).map(unit))
                    m <- nondets(lattice.getMessages(constructorArguments(1)).map(unit))
                yield lattice.envelope(Envelope(act, m))

            def get(v: Value) = nondets(lattice.getEnvelopes(v).map(unit))
            val fields = Map(
              "receiver" -> lift(ev => lattice.actor(ev.receiver)),
              "message" -> lift(ev => lattice.message(ev.message))
            )

        case object message extends Struct[Msg]:
            val structName: String = "message"
            def mk(constructorArguments: List[Value], idn: Identity): A[Value] =
                unit(lattice.message(MetaMessage(constructorArguments(0), constructorArguments(1))))
            def get(v: Value) = nondets(lattice.getMessages(v).map(unit))
            // We remove one argument from the field set as arguments and payload are the same
            override def fieldSize: Int = fields.keySet.size - 1
            def fields = Map(
              "tag" -> { m => m.tag },
              "arguments" -> { (message: Msg) => message.toMetaMessage.flatMap(_.vlus) },
              "payload" -> { message => message.toMetaMessage.flatMap(_.vlus) }
            )

    /** Override primitive application in order to include the meta primitives as well */
    override protected def applyPrimitives(app: App, fun: Val, ags: List[Val]): A[Val] =
        lattice.getPrimitives(fun).foldMapM { prm =>
            if MetaPrimitives.isPrimitive(prm) then MetaPrimitives.primitives(prm).call(ags, app.idn)
            else if primitives.allPrimitives.keySet.contains(prm) then super.applyPrimitive(app, primitives(prm), ags)
            else mbottom
        }

    /**
     * Evaluates the given expression to a valid mirror ref.
     *
     * If the returned value is a lambda it is applied with the given actor value, otherwise it is returned directly
     */
    protected def evalMirrorRef(mirrorRef: SchemeExp, actor: Value, actorExp: SchemeExp): A[Value] =
        eval(mirrorRef).flatMap { vlu =>
            log(s"+++ create-with-mirror mirror ref is $vlu")
            // synthesize a function call to use
            val app = SchemeFuncall(mirrorRef, List(actorExp), mirrorRef.idn)
            // Either apply the ufnction or return the mirror directly
            mjoin(
              applyClosures(app, vlu, List(actor)),
              nondets(lattice.getActors(vlu).map(lattice.actor andThen unit))
            )
        }

    /**
     * Catches an error returned from the mirror and records it according to MonadError
     *
     * @param idn
     *   the source location of where the error occured
     * @param vlu
     *   the value possibly containing the error
     */
    protected def catchErr(idn: Identity)(vlu: Value): A[Value] =
        if lattice.isError(vlu) then fail(MirrorError(vlu, idn)).map(_ => lattice.void) else unit(lattice.void)

    override def eval(exp: SchemeExp): A[Value] = exp match
        case ASchemeCreate(beh, ags, idn) =>
            // we intercept actor creation and forward it to the meta layer if necessary
            for
                evaluatedBeh <- nontail(eval(beh))
                evaluatedAgs <- evalAll(ags)
                result <- interceptCreate(evaluatedBeh, evaluatedAgs, lattice.bool(false), ags, idn)
            yield result
        case ASchemeSend(actorRef, Identifier(tag, _), ags, idn) =>
            for
                evaluatedActor <- nontail(eval(actorRef))
                evaluatedAgs <- evalAll(ags)
                self <- selfActor
                message = Message(tag, evaluatedAgs, ags)
                envelope = lattice.envelope(Envelope(self, reifyMessage(message, ags)))
                result <- intercept { mirror =>
                    /* , TODO: add the source location of the send here */
                    mirrorAsk(mirror.tid, "send", List(envelope, lattice.nil), self, idn) >>= catchErr(idn)
                } /* otherwise */ {
                    log(s"base/send $actorRef $tag $ags")
                    // TODO: chagne emptyContext to actual context
                    allocLst(ags.zip(evaluatedAgs)).flatMap { ags =>
                        sendMessage(evaluatedActor, lattice.symbol(tag), ags, emptyContext).map(_ => lattice.nil)
                    }
                }
            yield result

        case select @ ASchemeSelect(handlers, idn) =>
            // A message receive in the meta protocol works as follows:
            // If a mirror is installed, a message "receive" is sent using a reification of a message.
            // the receive should respond with a lambda that is supposed to execute the selected behavior when applied.
            // This lambda is applied in the same context as the sending actor, therefore not in the mirror.
            for
                // select the correct handler first
                msgWithContext <- receive
                (msg, context) = msgWithContext
                // see if there is an appropriate handler
                tag <- msg.tag
                args <- msg.vlus
                // reify the message in an abstract value
                reifiedMessage = lattice.message(MetaMessage(tag, args))
                // send the intercepted message to the mirror if necessary
                result <- intercept { mirror =>
                    for
                        self <- selfActor
                        //_ = log(s"++ meta/receive intercepting receive of $self with args $args")
                        env <- getEnv
                        beh <- currentBehavior
                        //_ = log(s"++ meta/receive Got current behavior $beh")
                        // TODO: provide sender and sender location
                        lam <- mirrorAsk(mirror.tid, "receive", List(reifiedMessage, lattice.beh(beh), lattice.nil, lattice.nil), self, idn)
                        //_ = log(s"++ meta/receive got handler from mirror $lam")
                        result <- applyThunk(lam, idn, args)
                    //_ = log(s"++ meta/receive result of running the mirror is $result")
                    yield result
                } /* otherwise */ {
                    //log(s"+++ meta/receive base looking up handler for $tag with args $args")
                    nondets(select.lookupHandler(tag).map(unit)).flatMap { case (pars, bdy) =>
                        log(s"+++ meta/receive found handler with ${pars.size} and $bdy for $tag")
                        args.take(primitives)(pars.size).flatMap { args =>
                            log(s"+++ meta/receive running handler with ${args.size} ${pars.size} for $tag")
                            withCtx(messageCtx(context)) { withEnvM(bindArgs(pars, args)) { eval(bdy) } }.map(res =>
                                log(s"+++ meta/receive result $res for $tag")
                                res
                            )
                        }
                    }
                }
            yield result

        // (create-with-mirror mirrorRef behavior)
        case SchemeFuncall(SchemeVar(Identifier("create-with-mirror", _)), mirrorRef :: behavior :: ags, idn) =>
            // TODO: in the concrete actor implementation we have that the create-with-mirror call
            // is also intercepted. We should do that here as well.
            for
                evaluatedBehavior <- nontail(eval(behavior))
                _ = log(s"+++ create-with-mirror (1) $behavior")
                evaluatedAgs <- nontail(evalAll(ags))
                _ = log(s"+++ create-with-mirror (2) $evaluatedAgs")
                // defer spawning the actor since the mirror will be installed later
                actorRef <- create(evaluatedBehavior, evaluatedAgs, idn, defer = true)
                actor = lattice.actor(actorRef)
                _ = log(s"+++ create-with-mirror (3) $actor")
                evaluatedMirrorRef <- evalMirrorRef(mirrorRef, actor, behavior)
                _ = log(s"+++ create-with-mirror (4) $evaluatedMirrorRef")
                _ <- nondets(lattice.getMirrors(evaluatedMirrorRef).map(mirror => installMirror(actor, mirror, strong = true)))
                _ = log(s"+++ intra mirror installed on $actor")
                // now spawn the actor once the mirror has been installed
                _ <- deferedSpawnActor(actorRef)
            yield actor

        case SchemeFuncall(SchemeVar(Identifier("base/send-envelope" | "send-envelope", _)), List(envelopeExpression), _) =>
            for
                evaluatedEnvelope <- nontail(eval(envelopeExpression))
                _ = log(s"+++ base/send-envelope $evaluatedEnvelope")
                // get the envelopes from the abstract domain
                envelope <- nondets(lattice.getEnvelopes(evaluatedEnvelope).map(unit))
                // get the receiver, and message from the envelope
                receiver = envelope.receiver
                message = envelope.message
                // send the message to the specified receiver
                // TODO: change emptyContext to actual context
                result <- send(receiver, message, emptyContext).map(_ => lattice.nil)
            yield result

        // For testing purposes
        case SchemeFuncall(SchemeVar(Identifier("error", _)), List(message), _) =>
            for
                m <- eval(message)
                _ = log(s"!! +++ intra error $m")
                result <- mbottom[Value]
            yield result

        case _ => super.eval(exp)
