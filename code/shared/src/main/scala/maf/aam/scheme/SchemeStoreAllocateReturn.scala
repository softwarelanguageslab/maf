package maf.aam.scheme

import maf.core.*
import maf.util.Trampoline.*

trait SchemeStoreAllocateReturn extends BaseSchemeAAMSemantics:
    case class RetAddr(kont: KonA) extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"RetAddr(${kont})"

    protected def allocRet(kont: KonA): Address =
      RetAddr(kont)

    override def ap(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Result =
        val addr = allocRet(kont)
        val (sto1, ext1) = writeStoV(sto, addr, value, ext)
        done(Set(SchemeState(Control.Ret(addr), sto1, kont, t, ext1)))
