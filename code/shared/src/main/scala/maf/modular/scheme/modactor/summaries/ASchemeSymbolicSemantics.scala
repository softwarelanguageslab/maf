package maf.modular.scheme.modactor.summaries

import maf.modular.scheme.modactor.*
import maf.language.scheme.*
import maf.core.Identifier
import maf.core.Monad
import maf.core.Monad.*
import maf.language.symbolic.*
import maf.language.symbolic.Symbolic.{Var => SymVar, *}

trait ASchemeSymbolicSemantics extends ASchemeSemantics:

    extension (s: SymVar)
        def tag: Symbolic =
            Ref(s, "tag")

    trait SymbolicSemanticsMonad[M[_]] extends ActorAnalysisM[A], SymbolicAllocator[M], SymbolicStoreM[M]:
        def symReceive: M[SymVar]
        def symArg(prm: Identifier): M[Value]
        def assert(s: Symbolic): M[Unit]
        def sym(v: Value): M[Symbolic]

    override val analysisM: SymbolicSemanticsMonad[A]

    import analysisM.*

    override def eval(e: SchemeExp): A[Value] = e match
        case ASchemeSend(actorRef, messageTpy, ags, idn) => ???
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

// class SimpleASchemeSymbolicSemantics extends ASchemeSymbolicSemantics
