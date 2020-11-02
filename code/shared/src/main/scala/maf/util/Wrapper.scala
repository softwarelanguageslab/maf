package maf.util

trait Wrapper[A, F] {
  def wrap(v: A): F
  def unwrap(f: F): A
}
