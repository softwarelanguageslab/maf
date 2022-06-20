package maf.modular.scheme.modactor

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
