package maf.analysis.store

import maf.util.Address
import maf.util.HMapKey
import cats.Show

trait StoreAddress extends Address, HMapKey
trait VectorAddress[V] extends StoreAddress:
    type Value = V
object VectorAddress:
    given [V]: Show[VectorAddress[V]] with
        def show(v: VectorAddress[V]): String = v.toString
trait PairAddress[V] extends StoreAddress:
    type Value = V
object PairAddress:
    given [V]: Show[PairAddress[V]] with
        def show(v: PairAddress[V]): String = v.toString
