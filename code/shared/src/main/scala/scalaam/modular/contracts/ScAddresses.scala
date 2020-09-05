package scalaam.modular.contracts

import scalaam.core.{Address, Identity}
import scalaam.language.contracts.ScIdentifier

sealed trait ScAddresses[+Context] extends Address
case class ScVarAddr[Context](id: ScIdentifier, context: Context) extends ScAddresses[Context] {

  /** Should the address be included when printing an environment or store?
    * This allows to reduce the size of the printed environment/store.
    * Address that are not printable may for example include addresses of primitive functions.
    */
  override def printable: Boolean = true

  /** The identity of an address *
    * Should correspond to the program location where the address was allocated *
    * Can be Identity.none if there is no sensible program location (e.g., pre-allocated addresses for primitives)
    */
  override def idn: Identity = id.idn

  override def toString: String = s"var ($id)"
}
