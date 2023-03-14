package maf.syntax

abstract class Expression[+E <: Expression[E]]:
  def idn: Identity
  def subexpressions: List[Expression[E]]
  def fv: Set[String]
  val height: Int = 0
