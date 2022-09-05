package maf.modular.switch

import maf.modular.AnalysisEntry
import maf.language.scheme.*
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.util.benchmarks.Timeout.T

/** This class represents an analysis S that can switch to an analysis Sp */
trait SwitchSemantics[S, Sp]:
    /** The type of the state in the semantics of S */
    type State

    /** Inject the state of S into the semantics of Sp by creating a new instance of Sp with the state injected in it */
    def inject(s: State): Sp

trait StepSemantics[S]:
    type State

    /** A way to run the analysis for a single step and obtain its state */
    def step(anl: S, s: State): (State, Boolean)

type SwitchSemantics_[S] = [Sp] =>> SwitchSemantics[S, Sp]

/**
 * An analysis that is able to switch between one or more analyses.
 *
 * This process is `Monad` based. It expects to be able to morph one monad into another, while also morphing one analysis state into another.
 */
trait SwitchableSemanticsAnalysis[S: SwitchSemantics_[Sp], Sp: SwitchSemantics_[S]](exp: SchemeExp, semanticsA: S, semanticsB: Sp)
    extends AnalysisEntry[SchemeExp]:

    override def finished: Boolean = ???

    override def analyzeWithTimeout(timeout: T): Unit = ???
