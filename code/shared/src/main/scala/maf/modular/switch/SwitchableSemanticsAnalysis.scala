package maf.modular.switch

import maf.modular.AnalysisEntry
import maf.language.scheme.*
import scala.reflect.ClassTag
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

    /** Returns the initial state of the semantics */
    def initialState(sme: S): State

type SwitchSemantics_[S] = [Sp] =>> SwitchSemantics[S, Sp]

/** An analysis that is able to switch between two other semantics */
trait SwitchableSemanticsAnalysis[S: SwitchSemantics_[Sp], Sp: SwitchSemantics_[S]](
    using StepSemantics[S],
    StepSemantics[Sp]
  )(using ClassTag[S],
    ClassTag[Sp])
    extends AnalysisEntry[SchemeExp]:

    val analysisA: S
    val analysisB: Sp

    protected val stepA: StepSemantics[S] = summon[StepSemantics[S]]
    protected val stepB: StepSemantics[Sp] = summon[StepSemantics[Sp]]

    /** The semantics that we are currently running */
    // By default analysisA runs first
    protected val initialSemantics: S | Sp = analysisA

    /** Returns the initial state of the analysis */
    private def initialState: stepA.State | stepB.State = initialSemantics match
        case (_: S)  => stepA.initialState(analysisA)
        case (_: Sp) => stepB.initialState(analysisB)
        case _       => throw new Exception("invalid semantics given")

    private var _finished = false

    private var _result: Any = null

    override def finished: Boolean = _finished

    /** Chooses whether to switch to a different semantics. Will keep the current semantics by default. */
    protected def switch(sem: S | Sp, st: stepA.State | stepB.State): (S | Sp, stepA.State | stepB.State) =
        (sem, st)

    override def analyzeWithTimeout(timeout: T): Unit =
        /** Loop until the current analysis has no more step */
        def loop(sem: S | Sp, st: stepA.State | stepB.State): Unit =

            val (nextSt: (stepA.State | stepB.State), hasFinished) = (sem, st) match
                case (a: S, st: stepA.State @unchecked) =>
                    stepA.step(a, st)
                case (b: Sp, st: stepB.State @unchecked) =>
                    stepB.step(b, st)
                case (_, _) => throw new Exception("Incompatible types")

            if hasFinished then
                _finished = true
                _result = nextSt
            else
                val (nextSem, nextStP) = switch(sem, nextSt)
                loop(nextSem, nextStP)

        loop(initialSemantics, initialState)
