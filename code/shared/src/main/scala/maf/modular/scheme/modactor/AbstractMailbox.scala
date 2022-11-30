package maf.modular.scheme.modactor

import maf.language.AScheme.ASchemeValues.Message
import maf.language.AScheme.ASchemeValues
import maf.util.datastructures.SmartUnion
import maf.util.Default
import maf.core.Lattice

/**
 * An abstract representation of a mailbox.
 *
 * @tparam M
 *   the type of the messages in the mailbox
 * @tparam K
 *   the type of context of the messages in the mailbox
 */
trait AbstractMailbox[M, K]:
    /**
     * Enqueue a message in the mailbox.
     *
     * @param msg
     *   the message to enqueue in the mailbox
     * @return
     *   an updated abstract mailbox
     * @note
     *   implementations of this method may assume that the a sequence of <code>enqueue</code> commands entails a sending order of the messages in the
     *   mailbox.
     */
    def enqueue(msg: M, ctx: K): AbstractMailbox[M, K]

    /**
     * Pop a message from the mailbox. Depending on the level of abstraction used, it can return a set of messages that might be received during that
     * turn.
     */
    def dequeue: Set[((M, K), AbstractMailbox[M, K])] // TODO: rename to dequeue

    /** Returns all messages in the mailbox */
    def messages: Set[M]

    /** Merge the given mailbox with the current one */
    def merge(other: AbstractMailbox[M, K]): AbstractMailbox[M, K]

trait AbstractMessage[K]:
    def tag: String

/** A map lattice based mailbox, which maps tags and contexts to payloads */
case class MapMailbox[K, M <: AbstractMessage[K]: Lattice](msgs: Map[(String, K), M]) extends AbstractMailbox[M, K]:
    def enqueue(msg: M, ctx: K): AbstractMailbox[M, K] =
        val tag = msg.tag
        val old = msgs.get((tag, ctx)).getOrElse(msg)
        this.copy(msgs = msgs + ((tag, ctx) -> Lattice[M].join(old, msg)))

    def messages: Set[M] = msgs.values.toSet

    def dequeue: Set[((M, K), AbstractMailbox[M, K])] =
        // for dequeue: no multiplicity and order so any message can be dequeued at any time
        // and the mailbox can still contain the same message.
        msgs.map { case ((_, ctx), m) => ((m, ctx), this) }.toSet

    def merge(other: AbstractMailbox[M, K]): AbstractMailbox[M, K] = this.copy(msgs = (other match
        case MapMailbox(msgs) =>
            (this.msgs.keys ++ msgs.keys).map { key =>
                key -> ((this.msgs.get(key), msgs.get(key)) match
                        case (Some(v), Some(w)) => Lattice[M].join(v, w)
                        case (None, Some(w))    => w
                        case (Some(v), None)    => v
                        case (None, None) =>
                            ???
                    // is impossible as we are iterating over the keys of both maps
                )
            }
    ).toMap)

/**
 * A powerset implementation of the mailbox, as used in Emanuele D’Osualdo, Jonathan Kochems, and Luke Ong. Automatic verification of erlang- style
 * concurrency. In Static Analysis - 20th International Symposium, SAS 2013
 */
case class PowersetMailbox[M, K](msgs: Set[(M, K)]) extends AbstractMailbox[M, K]:
    def enqueue(msg: M, ctx: K): PowersetMailbox[M, K] =
        this.copy(msgs = msgs + ((msg, ctx)))

    def dequeue: Set[((M, K), AbstractMailbox[M, K])] =
        // We return a copy of the same mailbox for each message since message multiplicity is unknown
        msgs.map(m => (m, PowersetMailbox(msgs)))

    val messages: Set[M] = msgs.map(_._1)

    def merge(other: AbstractMailbox[M, K]): AbstractMailbox[M, K] = other match
        case PowersetMailbox(msgs) => this.copy(msgs = SmartUnion.sunion(this.msgs, msgs))

object PowersetMailbox:
    given [M, K]: Default[PowersetMailbox[M, K]] with
        def default: PowersetMailbox[M, K] = PowersetMailbox(Set())

/** This trait implements simple messages where the arguments of the messages are not store allocated */
trait SimpleMessageMailbox extends SchemeModActorSemantics:
    type Msg = ASchemeValues.Message[Value]
    def mkMessage(tpy: String, arguments: List[Value]): Msg = Message(tpy, arguments)
    def getTag(msg: Msg): String = msg.tag
    def getArgs(msg: Msg): List[Value] = msg.vlus

/** An analysis with a powerset mailbox */
trait PowersetMailboxAnalysis extends SchemeModActorSemantics:
    def emptyMailbox: Mailbox = PowersetMailbox(Set())
