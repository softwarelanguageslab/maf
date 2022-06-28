package maf.modular.scheme.modactor

import maf.language.AScheme.ASchemeValues.Message

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
    def pop: Set[M]

    /** Remove the specified message from the mailbox */
    def remove(m: M): AbstractMailbox[M]

    /** Returns all messages in the mailbox */
    def messages: Set[M]

/**
 * A powerset implementation of the mailbox, as used in Emanuele D’Osualdo, Jonathan Kochems, and Luke Ong. Automatic verification of erlang- style
 * concurrency. In Static Analysis - 20th International Symposium, SAS 2013
 */
case class PowersetMailbox[M](msgs: Set[M]) extends AbstractMailbox[M]:
    def enqueue(msg: M): PowersetMailbox[M] =
        this.copy(msgs = msgs + msg)

    def pop: Set[M] = msgs

    def remove(m: M): PowersetMailbox[M] =
        // since the power set mailbox does not encode message multiplicity, removing a message is a no-op, since its multiplicity is not known and is therefore infinite.
        PowersetMailbox(msgs)

    val messages: Set[M] = msgs

/** This trait implements simple messages where the arguments of the messages are not store allocated */
trait SimpleMessageMailbox extends SchemeModActorSemantics:
    type Msg = Message[Value]
    def mkMessage(tpy: String, arguments: List[Value]): Msg = Message(tpy, arguments)
    def getTag(msg: Message[Value]): String = msg.tag
    def getArgs(msg: Message[Value]): List[Value] = msg.vlus

/** An analysis with a powerset mailbox */
trait PowersetMailboxAnalysis extends SchemeModActorSemantics:
    def emptyMailbox: Mailbox = PowersetMailbox(Set())
