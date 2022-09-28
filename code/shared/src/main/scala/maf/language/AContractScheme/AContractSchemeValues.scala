package maf.language.AContractScheme

object AContractSchemeValues:
    /**
     * A value to represents ensures/c contracts which ensures that message are being sent
     *
     * @param messages
     *   the set of messages that need to be sent by the annotated actor
     */
    case class EnsuresC[L](messages: List[MessageC[L]])

    /**
     * A value to represents message/c contracts.
     *
     * @tparam L
     *   the type of the values in the abstract domain
     */
    case class MessageC[L](tag: String, arguments: List[L], ensures: EnsuresC[L])
