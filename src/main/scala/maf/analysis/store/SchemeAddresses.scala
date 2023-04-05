package maf.analysis.store

import maf.syntax.Identity
import maf.util.Address
import maf.syntax.scheme.SchemeExp
import maf.syntax.Identifier

sealed trait SchemeAddress[+Context] extends Address
case class VarAddrWithContext[Context, V](vrr: Identifier[SchemeExp], ctx: Context) extends SchemeAddress[Context], VarAddress[V]:
    val idn: Identity = vrr.idn
    def printable: Boolean = true
case class PtrAddr[Context](exp: SchemeExp, ctx: Context) extends SchemeAddress[Context]:
    val idn: Identity = exp.idn
    def printable: Boolean = true
case class AddrWrapper[V](adr: Address) extends SchemeAddress[Nothing], ValAddress[V]:
    val idn: Identity = adr.idn
    def printable: Boolean = true
case class PrmAddr[V](nam: String) extends SchemeAddress[Nothing], ValAddress[V]:
    type Value = V
    def printable = false
    def idn: Identity = Identity.none
    override def toString: String = s"PrmAddr($nam)"
