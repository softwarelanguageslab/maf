package maf.core

package address {

  sealed trait BaseAddr extends Address { def printable = true; def idn: Identity }
  case class VarAddr(vrb: Identifier) extends BaseAddr { def idn: Identity = vrb.idn; override def toString = s"<variable $vrb>" }
  case class PtrAddr(exp: Expression) extends BaseAddr { def idn = exp.idn; override def toString = s"<pointer $exp>" }
  case class PrmAddr(nam: String) extends BaseAddr { def idn: Identity = Identity.none; override def toString = s"<primitive $nam>" }
}
