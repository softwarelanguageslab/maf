package maf.modular.scheme.modactor.summaries

import maf.modular.scheme.modactor.*
import maf.language.scheme.*
import maf.core.Address
import maf.core.Identifier
import maf.core.Monad
import maf.core.Monad.*
import maf.language.symbolic.*
import maf.language.symbolic.Symbolic.{Var => SymVar, *}
import monocle.Lens
import maf.core.monad.*
import maf.core.SetMonad.*
import maf.core.{MonadStateT, StateOps}

trait ASchemeSymbolicSemantics extends ASchemeSemantics:

    extension (s: SymVar)
        def tag: Symbolic =
            Ref(s, "tag")

    trait SymbolicSemanticsMonad[M[_]] extends ActorAnalysisM[A], SymbolicAllocator[M], SymbolicStoreM[M]:
        def symReceive: M[SymVar]
        def symArg(prm: Identifier): M[Value]
        def assert(s: Symbolic): M[Unit]
        def sym(v: Value): M[Symbolic]

        /**
         * Return the contracts for the given actor ref, if no information is available about the contracts associated with the message for the given
         * actorRef, then generate logical variables that can be instantiated later according to the given `siz`
         */
        def contracts(α: ActorRef, t: String, siz: Int): M[List[Symbolic]]

        /** Same as the one before but for the definition of a behavior */
        def contracts(β: Behavior): M[List[Symbolic]]

    // Symbolic predicates
    def isActor: Symbolic = VarId("actor?")
    def isBeh: Symbolic = VarId("behavior?")

    override val analysisM: SymbolicSemanticsMonad[A]

    import analysisM.*

    override def eval(e: SchemeExp): A[Value] = e match
        case ASchemeSend(α, t, as, idn) =>
            //   actor? α ∧ φ₁ a₁ ∧ φ₂ a₂ ∧ … ∧ φₖ aₖ
            // -------------------------------------- [Send-Safe]
            //            send α t α₁ α₂
            //  forall (φ₁, φ₂, ..., φₖ) ∈ contracts(α, t)
            //
            //  meaning that the actor reference should satisfy the `actor?` predicate
            //  and the arguments satisfy the contracts corresponding to the message.
            for
                αv <- eval(α)
                // evaluate the actor to gain some information about its contracts
                `α′` <- nondets(lattice.getActors(αv).map(unit))
                // assert that the actor is an actor
                _ <- sym(αv) >>= (actor => assert(isActor(actor)))
                // retrieve the contracts for the given message
                φs <- contracts(`α′`, t.name, as.size)
                vs <- Monad.sequence(φs.zip(as).map { case (φ, a) =>
                    for
                        // evaluate each argument
                        v <- eval(a)
                        // obtain their symbolic representation
                        s <- sym(v)
                        // assert that the contract holds on the argument
                        _ <- assert(φ(s))
                    yield v
                })
                _ <- mkMessage(t.name, vs) >>= send_(`α′`)
            yield lattice.nil

        case ASchemeBecome(β, as, idn) =>
            //    behavior? β ∧ φ₁ a₁ ∧ φ₂ a₂ ∧ … ∧ φₖ aₖ
            //   ---------------------------------------- [Become-Safe]
            //                become β a₁ a₂ …
            //    forall (φ₁, φ₂, …) ∈ contracts(β)
            for
                // evaluate the behavior
                βv <- eval(β)
                // get the behavior values from the behavior
                `β′` <- nondets(lattice.getBehs(βv).map(unit))
                // assert that the behavior is indeed a behavior
                _ <- sym(βv) >>= (beh => assert(isBeh(beh)))
                // retrieve the contracts associated with the constructor of the given behavior
                φs <- contracts(`β′`)
                // evaluate the arguments and assert that the contracts hold for them
                vs <- Monad.sequence(φs.zip(as).map { case (φᵢ, aᵢ) =>
                    for {
                        v <- eval(aᵢ)
                        s <- sym(v)
                        _ <- assert(φᵢ(s))
                    } yield v
                })
                // become the behavior
                _ <- become(`β′`, vs, idn)
            yield lattice.nil

        // Symbolically executing a select means executing
        // all possible branches in that select.
        case ASchemeSelect(handlers, idn) =>
            for
                m <- symReceive
                handler <- nondets(handlers.toList.map(unit))
                (tag, (prs, bdy)) = handler
                // assert that the current handler is for the given tag
                _ <- assert(m.tag === Lit(tag))
                // inject symbolic values for the parameters
                vlus <- prs.mapM(symArg)
                // bind the parameters on the correct store locations
                result <- withEnvM(bindArgs(prs, vlus)) {
                    Monad.sequence(bdy.map(eval))
                } >>= trace(s"actor/recv $m result")
            //
            yield lattice.nil

        case _ => super.eval(e)

/**
 * A stack of monads for building a working analysis
 *
 * @tparam M
 *   the type of the inner monad, can be changed to change how the monad deals with non-determinism.
 */
trait MonadStack[M[_]: Monad] extends ASchemeSemantics:
    /** The type of state of the analysis */
    type S

    /** The type of the reader component of the analysis */
    type R

    /** The type of the mailbox */
    type Mailbox <: AbstractMailbox[Msg]

    /** Represents a set of lenses for manipulating the state */
    case class State(
        mailboxes: Lens[S, Map[ActorRef, Mailbox]],
        store: Lens[S, Map[Address, Value]],
        symStore: Lens[S, Map[Address, Symbolic]])

    /** Represents a set of lenses for manipulating the reader component of the analysis */
    case class ReaderL(
        environment: Lens[R, Env],
        context: Lens[R, Ctx])

    /**
     * The monad needs to pass down information using a reader but needs to keep track of a mutable store and mailboxes hence the state monad .The
     * EitherT monad is used to keep errors into account.
     */
    type Reader = [Y] =>> ReaderT[EitherT_[State][M], ReaderL, Y]
    type A[X] = MonadStateT[State, Reader, X]

    private val monadInst: StateOps[State, A] = MonadStateT.stateInstance[State, Reader]

    given m: ActorAnalysisM[A] with
        export monadInst.*

    override val analysisM: ActorAnalysisM[A] = m

/** Then we can implement a large part of the AnalysisM and ActorAnalysisM monads */

class SimpleASchemeSymbolicSemantics extends ASchemeSymbolicSemantics, ASchemeConstantPropagationDomain, MonadStack[Set]
