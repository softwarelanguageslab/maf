package maf.util.datastructures

case class MultiSet[X](content: Map[X, Int], cardinality: Int):
    def getMult(elm: X) = content.getOrElse(elm, 0)
    def updateMult(elm: X)(update: Int => Int) =
        val mult = getMult(elm)
        val updated = update(mult)
        if updated <= 0 then MultiSet(content - elm, cardinality - mult)
        else MultiSet(content + (elm -> updated), cardinality - mult + updated)
    def contains(elm: X) = getMult(elm) > 0
    def +(elm: X) = updateMult(elm)(_ + 1)
    def -(elm: X) = updateMult(elm)(_ - 1)
    def ++(ms: MultiSet[X]) = combine(ms)(_ + _)
    def --(ms: MultiSet[X]) = combine(ms)(_ - _)
    def combine(ms: MultiSet[X])(f: (Int, Int) => Int) =
      ms.content.foldLeft(this) { case (acc, (elm, cnt)) =>
        acc.updateMult(elm)(f(_, cnt))
      }
    def removeAll(elm: X) =
      MultiSet(content - elm, cardinality - getMult(elm))
    def distinctElements = content.keys
    def distinctCount = content.size
    def toList = content.toList
    def toSet = content.keySet
    def toMap = content
    def groupBy[K](key: X => K): Map[K, MultiSet[X]] =
      this.content.foldLeft(Map.empty[K, MultiSet[X]]) { case (acc, (elm, cnt)) =>
        val k = key(elm)
        val m = acc.getOrElse(k, MultiSet.empty)
        acc + (k -> MultiSet(m.content + (elm -> cnt), m.cardinality + cnt))
      }
object MultiSet:
    def empty[X] = MultiSet[X](Map.empty, 0)
    def apply[X](map: Map[X, Int]): MultiSet[X] = MultiSet(map, map.values.sum)
