package maf.modular.scv

import maf.core.{Address, Identity}
import maf.modular.scheme.ExceptionAddr

case class ScvExceptionAddr[Component](component: Component, idn: Identity) extends ExceptionAddr[Component]:
    def printable: Boolean = true
