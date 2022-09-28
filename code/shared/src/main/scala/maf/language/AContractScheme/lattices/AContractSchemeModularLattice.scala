package maf.language.AContractScheme.lattices

import maf.language.AScheme.lattices.*
import maf.core.*
import maf.lattice.interfaces.*
import maf.language.AContractScheme.AContractSchemeValues.*
import maf.language.AContractScheme.AContractSchemeLattice
import maf.lattice.AbstractSetType
import maf.lattice.HMap

class AContractSchemeModularLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends ASchemeModularLattice[A, S, B, I, R, C, Sym],
      AContractSchemeLattice[HMap, A]:

    object EnsuresCT extends AbstractSetType[EnsuresC[L], EnsuresCTs]:
        def wrap = EnsuresCTs.apply

    object MessageCT extends AbstractSetType[MessageC[L], MessageCTs]:
        def wrap = MessageCTs.apply

    case class EnsuresCTs(ensures: Set[EnsuresC[L]]) extends Value, Product1[Set[EnsuresC[L]]]:
        def ord = 33
        val tpy = EnsuresCT
        def typeName = "ENS"
        // TODO[easy]: implement a better string representation that gives additional information
        override def toString: String = s"<ensure/c>"

    case class MessageCTs(messages: Set[MessageC[L]]) extends Value, Product1[Set[MessageC[L]]]:
        def ord = 34
        val tpy = MessageCT
        def typeName = "MSC"
        // TODO[easy]: implement a better string representation that gives additional information
        override def toString: String = s"<message/c>"

    /** Inject an ensures/c value in the abstract domain */
    def ensuresC(nww: EnsuresC[L]): L =
        HMap.injected(EnsuresCT, nww)

    /** Inject a message/c value in the abstract domain */
    def messageC(nww: MessageC[L]): L =
        HMap.injected(MessageCT, nww)

    /** Extract ensures/c values from the abstract domain */
    def getMessageC(x: L): Set[MessageC[L]] =
        x.get(MessageCT)

    /** Extract ensures/c values from the abstract domain */
    def getEnsuresC(x: L): Set[EnsuresC[L]] =
        x.get(EnsuresCT)
