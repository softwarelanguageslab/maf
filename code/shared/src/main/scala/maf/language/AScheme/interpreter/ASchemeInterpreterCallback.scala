package maf.language.AScheme.interpreter

import maf.language.AScheme.ASchemeValues.*
import maf.core.Identity
import maf.language.AScheme.interpreter.ConcreteASchemeValues.ConcreteActor

/** A collection of callbacks invoked by `CPSASchemeInterpreter` can be used to build soundness and precision tests */
trait ASchemeInterpreterCallback:
    import maf.language.ContractScheme.interpreter.ConcreteValues.Value
    type M = Message[Value]

    /** Called when an actor sends a message */
    def sendMessage(receiver: ConcreteActor, msg: M, idn: Identity): Unit

    /** Called when `self` changes its behavior using a `become` statement */
    def become(self: ConcreteActor, beh: Behavior): Unit

    /** Called when a new actor is spawned */
    def spawn(newActor: ConcreteActor): Unit

object ASchemeInterpreterCallback:
    /**
     * An empty implementation for `ASchemeInterpreterCallback`.
     *
     * Use this class if you want to override certain callback behaviors
     */
    class EmptyCallback extends ASchemeInterpreterCallback:
        def sendMessage(receiver: ConcreteActor, msg: M, idn: Identity): Unit = ()
        def become(self: ConcreteActor, beh: Behavior): Unit = ()
        def spawn(newActor: ConcreteActor): Unit = ()

    object EmptyCallback extends EmptyCallback
