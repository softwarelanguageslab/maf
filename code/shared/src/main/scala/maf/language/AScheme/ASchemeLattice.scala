package maf.language.AScheme

import maf.core.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.AScheme.ASchemeValues.{Actor, Behavior, Future, Message}

trait ASchemeLattice[L, A <: Address] extends SchemeLattice[L, A]:
    type ReifiedMessage = Message[L]

    /** Injection of an actor */
    def actor(actor: Actor): L

    /** Injection of a future */
    def future(future: Future): L

    /** Injection of an actor behavior */
    def beh(behavior: Behavior): L

    /** Extract the actors in this value */
    def getActors(x: L): Set[Actor]

    /** Extract the futures in this value */
    def getFutures(x: L): Set[Future]

    /** Extract actor behaviors in this value */
    def getBehs(x: L): Set[Behavior]

    /** Extract reified message from this value */
    def getMessages(x: L): Set[ReifiedMessage]

    /** Inject a messages in the abstract domain */
    def message(m: ReifiedMessage): L
