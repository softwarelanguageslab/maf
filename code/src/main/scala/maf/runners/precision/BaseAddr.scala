package maf.runners.precision

import maf.domain.*
import maf.syntax.*

package address {

  sealed trait BaseAddr extends Address { def printable = true; def pos: Option[Position] }
  case class VarAddr(vrb: Identifier) extends BaseAddr { def pos: Option[Position] = vrb.pos; override def toString = s"<variable $vrb>" }
  case class PtrAddr(exp: Expression) extends BaseAddr { def pos = exp.pos; override def toString = s"<pointer $exp>" }
  case class PrmAddr(nam: String) extends BaseAddr { def pos: Option[Position] = None; override def toString = s"<primitive $nam>" }
}
