package maf.util

object StringUtil:

    /** Allows adding sequence numbers to Strings. Sequence numbers start from 1. */
    trait NumberedStrings:

        /** The last used sequence number for messages. */
        private var count: Int = 0

        /** Adds a line number to the given string. */
        def addSequenceNumber(s: String): String =
            count += 1
            s"${toWidth(count.toString + ".", 5)} $s"

        /** Resets the numbering. */
        def resetNumbering(): Unit = count = 0

    /**
     * Ensures a string has the given width by filling with whitespaces. If the string is longer than the given width, the string will be returned.
     */
    def toWidth(s: String, w: Int): String = s + (" " * (w - s.length))
