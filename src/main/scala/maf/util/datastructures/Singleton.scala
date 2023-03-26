package maf.util.datastructures

class Singleton[V](v: V) extends Set[V]:
  private val singletonSet: Set[V] = Set(v)
  export singletonSet.*
