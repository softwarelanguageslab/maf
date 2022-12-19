package maf.language.racket

import maf.language.scheme.*
import maf.lattice.interfaces.*
import maf.language.scheme.lattices.*
import maf.core.{Address, Expression, Identifier, Identity}
import maf.util.Reader
import javax.lang.model.element.ModuleElement.ProvidesDirective
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.AScheme.ASchemeParser
import maf.lattice.HMap
import maf.lattice.AbstractSetType

import maf.modular.scheme.SchemeDomain

trait RacketDomain extends SchemeDomain:
    implicit override lazy val lattice: RacketSchemeLattice[Value, Address]

/**
 * A value representation for Racket modules. It consists of a mapping between provided identifiers, and values bound to these identifiers
 *
 * @tparam Value
 *   the type of the abstract values bound to the identifiers in the module
 * @param vlus
 *   a mapping from identifiers to their values in the module
 */
case class RMod[Value](vlus: Map[String, Value], name: Option[String]):
    def lookup(nam: String): Value =
        vlus.get(nam).getOrElse(throw new Exception(s"$nam not found in module $name"))

/** Add Racket module values to the scheme lattice */
trait RacketSchemeLattice[L, A <: Address] extends SchemeLattice[L, A]:
    /** Inject a module in the abstract value domain */
    def rmod(mod: RMod[L]): L

    /** Extract the racket modules from the abstract values */
    def rmods(mod: L): Set[RMod[L]]

/** Add Racket module values to the module scheme lattice */
class RacketModularSchemeLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym],
      RacketSchemeLattice[HMap, A] {

    object RModT extends AbstractSetType[RMod[HMap], RMods]:
        def wrap = RMods.apply

    case class RMods(mods: Set[RMod[HMap]]) extends Value, Product1[Set[RMod[HMap]]]:
        def ord = 34
        def typeName: String = "RMOD"
        val tpy = RModT
        override def toString(): String = s"<mod: $mods>"

    override def rmods(mod: HMap): Set[RMod[HMap]] = mod.get(RModT)
    override def rmod(mod: RMod[HMap]): HMap = HMap.injected(RModT, mod)

}
