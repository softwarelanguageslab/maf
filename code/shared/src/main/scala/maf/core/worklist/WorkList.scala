package maf.core.worklist

/** Generic interface for a work list. */
trait WorkList[X] {
  def head: X
  def tail: WorkList[X]

  def add(x: X): WorkList[X]
  def addAll(xs: Iterable[X]): WorkList[X]
  def  ++(xs: Iterable[X]): WorkList[X] = addAll(xs)

  def map[Y]   (f: X =>       Y): WorkList[Y]
  def filter   (f: X => Boolean): WorkList[X]
  def filterNot(f: X => Boolean): WorkList[X]

  def  isEmpty: Boolean
  def nonEmpty: Boolean

  def toList: List[X]
  def toSet:   Set[X]
}

// Default implementation used in worklistMonoid.
object WorkList {
  def empty[X]: WorkList[X]                       = LIFOWorkList.empty
}