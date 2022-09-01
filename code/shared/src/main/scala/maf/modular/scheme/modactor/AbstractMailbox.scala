package maf.modular.scheme.modactor

import maf.language.AScheme.ASchemeValues.Message
import maf.language.AScheme.ASchemeValues

/**
 * An abstract representation of a mailbox.
 *
 * @tparam M
 *   the type of the messages in the mailbox
 */
trait AbstractMailbox[M]:
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
    def enqueue(msg: M): AbstractMailbox[M]

    /**
     * Enqueues a list of messages
     *
     * @param msgs
     *   the list of messages to enqueue in the mailbox
     * @return
     *   an updated mailbox.
     */
    def enqueue(msgs: List[M]): AbstractMailbox[M] =
        msgs.foldLeft(this)((mailbox, msg) => mailbox.enqueue(msg))

    /**
     * Pop a message from the mailbox. Depending on the level of abstraction used, it can return a set of messages that might be received during that
     * turn.
     */
    def dequeue: Set[(M, AbstractMailbox[M])] // TODO: rename to dequeue

    /** Returns all messages in the mailbox */
    def messages: Set[M]

/**
 * A powerset implementation of the mailbox, as used in Emanuele D’Osualdo, Jonathan Kochems, and Luke Ong. Automatic verification of erlang- style
 * concurrency. In Static Analysis - 20th International Symposium, SAS 2013
 */
case class PowersetMailbox[M](msgs: Set[M]) extends AbstractMailbox[M]:
    def enqueue(msg: M): PowersetMailbox[M] =
        this.copy(msgs = msgs + msg)

    def dequeue: Set[(M, AbstractMailbox[M])] =
        // We return a copy of the same mailbox for each message since message multiplicity is unknown
        msgs.map(m => (m, PowersetMailbox(msgs)))

    val messages: Set[M] = msgs

/** This trait implements simple messages where the arguments of the messages are not store allocated */
trait SimpleMessageMailbox extends SchemeModActorSemantics:
    type Msg = ASchemeValues.Message[Value]
    def mkMessage(tpy: String, arguments: List[Value]): Msg = Message(tpy, arguments)
    def getTag(msg: Msg): String = msg.tag
    def getArgs(msg: Msg): List[Value] = msg.vlus

/** An analysis with a powerset mailbox */
trait PowersetMailboxAnalysis extends SchemeModActorSemantics:
    def emptyMailbox: Mailbox = PowersetMailbox(Set())
