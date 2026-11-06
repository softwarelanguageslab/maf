package maf.domain

import maf.syntax.*
import maf.util.SmartHash

/** An address */
trait Address extends SmartHash:

    /**
     * Should the address be included when printing an environment or store? This allows to reduce the size of the printed environment/store. Address
     * that are not printable may for example include addresses of primitive functions.
     */
    def printable: Boolean

    /**
     * The program location where the address was allocated, or `None` if there is no sensible program location (e.g., pre-allocated addresses for
     * primitives)
     */
    def pos: Option[Position]
