package maf.syntax

/** An identifier. It has a name and a position */
case class Identifier[E <: Expression[E]](name: String, idn: Identity)
    extends Expression[E]:
  def fullString: String = s"$name@$idn"
  override def toString: String = name
  def fv: Set[String] = Set(name)
  override val height: Int = 0
  def subexpressions: List[E] = List()
  def to[E <: Expression[E]]: Identifier[E] = Identifier[E](name, idn)
  // TODO: not sure why this exists:
  // override lazy val hash: Int = (label, name).hashCode()
