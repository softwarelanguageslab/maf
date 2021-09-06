package maf.modular.scv

import maf.core.{Address, Identity}

case class ScvExceptionAddr[Component](component: Component, idn: Identity) extends Address:
    def printable: Boolean = true
