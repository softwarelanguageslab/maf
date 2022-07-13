package maf.modular.scheme.modactor

import maf.language.AScheme.ASchemeValues.Behavior
import maf.core.Monad.MonadSyntaxOps
import maf.core.Identifier
import maf.language.scheme.*
import maf.modular.scheme.modf.SchemeModFComponent
import maf.language.AScheme.ASchemeValues.ActorWaitCompleteFuture

trait SchemeModActorFutures extends SchemeModActorSemantics:
    inter =>

    abstract class FuturesInnerModF(intra: ModActorIntra, beh: Behavior) extends InnerModF(intra, beh):
        modf =>

        override def intraAnalysis(component: SchemeModFComponent): FuturesInnerModFIntra =
            FuturesInnerModFIntra(component)

        class FuturesInnerModFIntra(component: modf.Component) extends InnerModFIntra(component):
            import evalM.*

            override def eval(exp: SchemeExp): AEvalM[Value] = exp match
                case ASchemeAwait(future, _) =>
                    for
                        evaluatedFuture <- eval(future)
                        result <- nondets(lattice.getFutures(evaluatedFuture).map {
                            case ActorWaitCompleteFuture(aid) =>
                                unit(readAddr(ActorWaitFutureResolveAddr(intra.actorIdComponent(aid))))
                            case _ => mzero
                        })
                    // TODO: yield an error if nondets is applied on an empty set?, but the abstract value is not bottom?
                    yield result

                case SchemeFuncall(SchemeVar(Identifier("wait-for-termination", _)), List(actorRef), _) =>
                    for
                        actor <- eval(actorRef)
                        future <- nondets(lattice.getActors(actor).map { actor =>
                            unit(lattice.future(ActorWaitCompleteFuture(intra.actorIdComponent(actor.tid))))
                        })
                    yield future

                case SchemeFuncall(SchemeVar(Identifier("terminate", _)), List(e), _) =>
                    for
                        v <- eval(e)
                        _ = intra.notifyFutures(v)
                    yield lattice.nil

                case SchemeFuncall(SchemeVar(Identifier("terminate", _)), List(), _) =>
                    intra.notifyFutures(lattice.nil)
                    unit(lattice.nil)

                case _ => super.eval(exp)
