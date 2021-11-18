package maf.util.datastructures

case class SmartMap[K, V](content: Map[K, V], hc: Long) extends Iterable[(K, V)]:
    override def hashCode: Int = hc.toInt
    override def equals(other: Any): Boolean =
      other match
          case SmartMap(otherContent, otherHc) => hc == otherHc && content == otherContent
          case _                               => false
    def apply(adr: K): V = content(adr)
    def get(adr: K): Option[V] = content.get(adr)
    def getOrElse(adr: K, els: => V): V = content.getOrElse(adr, els)
    def +(bnd: (K, V)) =
      content.get(bnd._1) match
          case None      => SmartMap(content + bnd, hc + bnd.hashCode)
          case Some(old) => SmartMap(content + bnd, hc - (bnd._1, old).hashCode + bnd.hashCode)
    def ++(bds: Iterable[(K, V)]) =
      bds.foldLeft(this)((acc, bnd) => acc + bnd)
    def -(key: K) =
      content.get(key) match
          case None      => this
          case Some(vlu) => SmartMap(content - key, hc - (key, vlu).hashCode)
    def --(kys: Iterable[K]) =
      kys.foldLeft(this)((acc, key) => acc - key)
    def iterator: Iterator[(K, V)] = content.iterator
    def contains(key: K) = content.contains(key)
    def keys: Iterable[K] = content.keys
    def keySet: Set[K] = content.keySet
    // FOR TESTING PURPOSES
    private def computeHash: Int = content.map(_.hashCode).sum
// assert(computeHash == hashCode) <- enable to test

object SmartMap:
    def empty[K, V]: SmartMap[K, V] = SmartMap(Map.empty, 0)
    def apply[K, V](bnd: (K, V)): SmartMap[K, V] = SmartMap(Map(bnd), bnd.hashCode)
