package maf.aam.scheme

import maf.core.*

trait SchemeStoreAllocateReturn extends SchemeAAMSemantics:
    case class RetAddr(kont: Address) extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"RetAddr(${kont})"

    protected def allocRet(kont: Address): Address =
      RetAddr(kont)

    override def retVal(value: Val, sto: Sto, kont: Address, t: Timestamp, ext: Ext): State =
        val addr = allocRet(kont)
        val sto1 = writeStoV(sto, addr, value)
        SchemeState(Control.Ret(addr), sto1, kont, t, ext)
