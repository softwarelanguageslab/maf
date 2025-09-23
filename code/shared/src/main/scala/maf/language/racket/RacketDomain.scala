package maf.language.racket

import maf.language.scheme.*
import maf.lattice.interfaces.*
import maf.language.scheme.lattices.*
import maf.core.{Address, Expression, Identifier, Identity}
import maf.util.Reader
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.lattice.HMap
import maf.lattice.AbstractSetType

import maf.modular.scheme.SchemeDomain
import maf.modular.AbstractDomain

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

    def mapValues[W](f: Value => W): RMod[W] =
        this.copy(vlus = vlus.mapValues(f).toMap)
