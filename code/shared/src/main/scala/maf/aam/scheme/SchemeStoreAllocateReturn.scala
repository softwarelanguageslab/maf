package maf.aam.scheme

import maf.core.*

trait SchemeStoreAllocateReturn extends SchemeAAMSemantics:
    case class RetAddr(kont: KonA) extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"RetAddr(${kont})"

    private def allocRet(kont: KonA): Address =
      RetAddr(kont)

    override def ap(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): State =
        val addr = allocRet(kont)
        val sto1 = writeStoV(sto, addr, value)
        SchemeState(Control.Ret(addr), sto1, kont, t, ext)
