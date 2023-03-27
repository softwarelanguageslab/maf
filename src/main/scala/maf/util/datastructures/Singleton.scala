package maf.util.datastructures

object Singleton:
  def unapply[X](v: Set[X]): Option[X] =
    if v.size == 1 then Some(v.head) else None

class Singleton[V](v: V) extends Set[V]:
  private val singletonSet: Set[V] = Set(v)
  export singletonSet.*
