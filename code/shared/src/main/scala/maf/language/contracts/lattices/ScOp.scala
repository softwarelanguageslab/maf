package maf.language.contracts.lattices

import maf.language.scheme.lattices.SchemeOp
import maf.lattice.interfaces.Operation1
import maf.lattice.interfaces.Operation

sealed trait ScOp extends Operation

object ScOp {
  case class ScSchemeOp(op: SchemeOp) extends ScOp {
    val arity: Int = op.arity
    val name: String = op.name
    override def arityException[L](args: List[L]): String =
      op.arityException(args)
  }

  case object IsDependentContract extends Operation1("dependent-contract?") with ScOp
  case object IsNonZero extends Operation1("nonzero?") with ScOp
  case object IsPair extends Operation1("pair?") with ScOp
  case object IsMonitored extends Operation1("monitored?") with ScOp
}
