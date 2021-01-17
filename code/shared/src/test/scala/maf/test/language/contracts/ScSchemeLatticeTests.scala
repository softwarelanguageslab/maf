package maf.test.language.contracts

import maf.test.ScTests
import maf.modular.contracts.ScSchemeConstantPropagationDomain
import maf.util.ProductLattice
import maf.language.contracts.ScLattice.Blame
import maf.core.Identity
import maf.language.scheme.SchemeValue
import maf.language.sexp.Value
import maf.language.sexp.ValueInteger
import maf.language.sexp.ValueNil
import maf.language.contracts.ScLattice.Opq
import maf.core.Primitive
import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice
import maf.core.MayFail
import maf.core.Store

class ScSchemeLatticeTests extends ScTests with ScSchemeConstantPropagationDomain {
  val primMap = primitives.allPrimitives.map(p => (p.name, p)).toMap

  /** Apply the given operator without a store or an interpreter bridge */
  def applyOperator(
      name: String
    )(
      args: List[V]
    ): MayFail[(V, Store[Address, V]), maf.core.Error] = {
    val nilValue = SchemeValue(ValueNil, Identity.none)
    primMap(name).call(nilValue, args.map((nilValue, _)), null, null)
  }

  "Plus with opaque and non-bottom value" should "return opaque" in {
    val value = applyOperator("+")(List(lattice.opq(Opq()), schemeLattice.number(5)))
    value.map(v => lattice.isDefinitelyOpq(v._1)) shouldEqual MayFail.success(true)
  }
}
