package maf.modular.scheme.modactor

import maf.core.Monad.*
import maf.modular.scheme.modf.SchemeModFComponent
import maf.util.Monoid
import maf.util.MonoidImplicits.*
import maf.core.Monad
import maf.modular.scheme.modflocal.EffectsStateM
import maf.language.scheme.*
import maf.modular.ModAnalysis
import maf.language.AScheme.ASchemeLattice
import maf.core.Environment
import maf.core.Address
import maf.core.Identifier
import maf.modular.scheme.PtrAddr
import maf.core.Identity
import maf.modular.scheme.modf.SchemeModFComponent
import maf.language.scheme.primitives.SchemePrimitives
import maf.language.AScheme.ASchemeValues.Behavior
import maf.modular.scheme.VarAddr
import maf.util.benchmarks.Timeout.T
import scala.reflect.ClassTag
import maf.modular.Dependency
import maf.core.MonadStateT
import maf.core.monad.ReaderT
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.core.StateOps
import maf.core.Lattice
import maf.language.AScheme.ASchemeValues.AID

trait GlobalStoreModActor(prog: SchemeExp)
    extends SchemeModActorSemantics,
      SimpleMessageMailbox,
      PowersetMailboxAnalysis,
      FIFOWorklistAlgorithm[SchemeExp]:
    given componentClassTag: ClassTag[Component]

    case class Cmp(enclosingActor: SchemeModActorComponent[Context], innerModF: Option[SchemeModFComponent]) extends AID:
        def removeEnv: Cmp =
            Cmp(
              enclosingActor.removeEnv,
              innerModF match
                  case Some(SchemeModFComponent.Call(clo, ctx)) =>
                      Some(SchemeModFComponent.Call((clo._1, Environment.empty), ctx))
                  case v => v
            )

    type Context = Any
    type Component = Cmp

    case class IntraState(
        self: Component,
        writes: Set[Dependency] = Set(),
        reads: Set[Dependency] = Set(),
        calls: Set[Component] = Set(),
        mailboxes: Map[Component, Mailbox] = Map(),
        actors: Set[Component] = Set(),
        behaviors: Set[Behavior] = Set(),
        sto: Map[Address, Value] = Map())

    case class InterState(
        mailboxes: Map[Component, Mailbox],
        actors: Set[Component],
        behaviors: Set[Behavior],
        sto: Map[Address, Value])

    override val sequentialSemantics: ModularASchemeSemantics = new ModularASchemeSemantics:
        import maf.core.SetMonad.*

        type Ctx = Context
        type State = IntraState
        type Reader = [Y] =>> ReaderT[Set, (Ctx, Environment[Address]), Y]
        type A[X] = MonadStateT[IntraState, Reader, X]
        given lens: ActorLens[IntraState] = ???

        protected val monadInstance: StateOps[IntraState, A] = MonadStateT.stateInstance[IntraState, Reader]
        implicit val analysisM = new ModularAnalysisM with EffectsStateM[A, Component, IntraState] {
            export monadInstance.*
            import monadInstance.*
            import maf.core.monad.MonadLift.*
            def getEnv: A[Env] = map(lift(ReaderT.ask))(_._2)
            def getCtx: A[Ctx] = map(lift(ReaderT.ask))(_._1)
            def selfActor: A[ActorRef] = ???
            def selfActorCmp: A[Component] =
                get.map(_.self)
            def mbottom[X]: A[X] =
                lift(ReaderT.lift(Set()))
            def mjoin[X: Lattice](x: A[X], y: A[X]): A[X] =
                // in this lattice we do not join
                nondets(List(x, y))
            def nondets[X](xs: Iterable[A[X]]): A[X] =
                MonadStateT((s) => ReaderT((e) => xs.toList.foldMap(_.run(s).runReader(e))))

            def withEnv[X](f: Env => Env)(blk: A[X]): A[X] = ???

            //MonadStateT((s) => ReaderT.local { case (ctx, env: Environment[Address]) => (ctx, f(env)) }(blk.run(s)))

            private val initialEnv: Environment[Address] = ???

            /**
             * Runs the analysis represented by `m` for the given `cmp`. It will start the analysis with the appropriate environment, but will not
             * initialize the store.
             */
            def run[X](cmp: Component, m: A[X]): Set[(X, IntraState)] =
                val (ctx, env) = cmp match
                    case Cmp(MainActor, None | Some(SchemeModFComponent.Main))            => (initialCtx, initialEnv)
                    case Cmp(Actor(beh, env, ctx), None | Some(SchemeModFComponent.Main)) => (ctx, env)
                    case Cmp(_, Some(SchemeModFComponent.Call(clo, ctx)))                 => (ctx, clo._2)

                val st = IntraState(self = cmp)
                val ev = (ctx, env)
                m.run(st).runReader(ev)

        }

    override def view(cmp: Component): SchemeModActorComponent[ComponentContext] = ???

    override def finished: Boolean = ???

    override def getTag(msg: Msg): String = ???

    override def intraAnalysis(component: Component): ModActorIntra =
        new ModActorIntra(component) {}
