package maf.test.language.contracts

import maf.test.ScTests
import maf.modular.contracts.ScSchemeConstantPropagationDomain
import maf.util.ProductLattice
import maf.language.contracts.ScLattice.Blame
import maf.core.Identity

class ScSchemeLatticeTests extends ScTests with ScSchemeConstantPropagationDomain {
  "Scheme Symbol" should "be in the abstract domain" in {
    println(schemeLattice.join(lattice.schemeLattice.real(0.3), schemeLattice.real(0.2)))
    println(schemeLattice.join(lattice.blame(Blame(Identity.none, Identity.none)), schemeLattice.real(0.3)))
  }
}
