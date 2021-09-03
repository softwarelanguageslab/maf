package maf.modular.scv

import maf.core.{Identity, Address}

case class ScvExceptionAddr[Component](component: Component, idn: Identity) extends Address {
  def printable: Boolean = true
}
