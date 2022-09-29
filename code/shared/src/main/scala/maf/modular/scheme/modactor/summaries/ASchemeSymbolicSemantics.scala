package maf.modular.scheme.modactor.summaries

import maf.modular.scheme.modactor.*
import maf.language.scheme.*
import maf.language.symbolic.*

trait ASchemeSymbolicSemantics extends ASchemeSemantics:

    trait SymbolicSemanticsMonad[M[_]] extends ActorAnalysisM[A], SymbolicAllocator[M]

    override val analysisM: SymbolicSemanticsMonad[A]

    override def eval(e: SchemeExp): A[Value] = ???
