package maf.language.AContractScheme

import maf.core.*
import maf.language.scheme.*
import maf.language.scheme.lattices.*
import maf.language.AContractScheme.AContractSchemeValues.*
import maf.language.AScheme.ASchemeLattice

/** A lattice that includes contract values for the actor language */
trait AContractSchemeLattice[L, A <: Address] extends ASchemeLattice[L, A]:
    /** Inject an ensures/c value in the abstract domain */
    def ensuresC(nww: EnsuresC[L]): L

    /** Inject a message/c value in the abstract domain */
    def messageC(nww: MessageC[L]): L

    /** Extract ensures/c values from the abstract domain */
    def getMessageC(x: L): Set[MessageC[L]]

    /** Extract ensures/c values from the abstract domain */
    def getEnsuresC(y: L): Set[EnsuresC[L]]
