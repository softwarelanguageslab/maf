package maf.modular.scheme.modactor.summaries

import maf.util.datastructures.*
import maf.util.datastructures.MapOps.*
import maf.language.symbolic.*
import maf.core.Monad
import maf.core.Monad.*

/** A guard is a condition described by a formula. */
case class Guard(formula: Formula)

/** A symbolic language for representing constraints on actor related properties */
object ActorSymbolic:

    /**
     * Symbolically represents a message handler.
     *
     * The message handler is represented by λm.formula, where m is a variable that will be replaced by the actual received message.
     */
    def handler[M[_]: SymbolicAllocator]: M[(Formula, Var)] = ???

/**
 * A summary of an actor described as a state machine that can change state based on the tag of the message
 *
 * @param states
 *   a set of states
 * @param transitions
 *   a set of labeled transitions
 */
case class ActorSM[S](states: Set[S], transitions: MapWithDefault[S, Set[(Guard, S)]], currentState: S):
    /** Cause a state transition from the current state to the given new state */
    def become(guard: Guard, newState: S): ActorSM[S] =
        this.copy(states = this.states + newState, transitions = this.transitions.update(currentState)(ts => ts + (guard -> newState)))

object ActorSM:
    /**
     * Creates an empty state machine with the given initial state
     *
     * @param initial
     *   the initial state of the actor
     */
    def empty[S](initial: S): ActorSM[S] =
        ActorSM(Set(), Map[S, Set[(Guard, S)]]().useDefaultValue(Set()), initial)
