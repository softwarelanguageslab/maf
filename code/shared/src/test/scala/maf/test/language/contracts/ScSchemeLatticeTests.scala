package maf.test.language.contracts

import maf.test.ScTests
import maf.modular.contracts.ScSchemeConstantPropagationDomain
import maf.util.ProductLattice

class ScSchemeLatticeTests extends ScTests with ScSchemeConstantPropagationDomain {
  "Scheme Symbol" should "be in the abstract domain" in {
    println(lattice.join(lattice.real(0.3), lattice.real(0.2)))
  }
}
