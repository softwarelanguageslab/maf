package maf.util

object SingletonSet {
  def apply[A](v: A): Set[A] = Set(v)
}
