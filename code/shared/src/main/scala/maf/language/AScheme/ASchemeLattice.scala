package maf.language.AScheme

import maf.core.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.AScheme.ASchemeValues.{AbstractMessage, Actor, Behavior, Future}
import maf.modular.scheme.modactor.MirrorValues.Mirror
import maf.modular.scheme.modactor.MirrorValues.Envelope
import maf.language.AScheme.ASchemeValues.MetaMessage

trait ASchemeLattice[L, A <: Address] extends SchemeLattice[L, A]:
    case class Error(vlu: L)

    type ReifiedMessage = AbstractMessage[L]

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

    /** Extract mirrors from this value */
    def getMirrors(x: L): Set[Mirror[Actor]]

    /** Extract the envelopes from the value */
    def getEnvelopes(x: L): Set[Envelope[Actor, L]]

    /** Inject a messages in the abstract domain */
    def message(m: ReifiedMessage): L

    /** Injects a mirror in the abstract domain */
    def mirrors(m: Mirror[Actor]): L

    /** Injects an envelope in the abstract domain */
    def envelope(e: Envelope[Actor, L]): L

    /** Injects an error message in the abstract domain */
    def error(e: L): L

    /** Returns the errors in the abstract value */
    def getErrors(e: L): Set[Error]

    /** Returns true if the value might be an error */
    def isError(e: L): Boolean = getErrors(e).nonEmpty
