package maf.modular.scv.actors

import maf.modular.scheme.modactor.*
import maf.core.Monad
import maf.core.Monad.*
import maf.language.AContractScheme.*
import maf.language.AContractScheme.AContractSchemeValues.*
import maf.language.scheme.*

/** Semantics for evaluating contract scheme for actors */
trait AContractSchemeSemantics extends ASchemeSemantics, AContractSchemeDomain:
    override def eval(e: SchemeExp): A[Value] = ???
