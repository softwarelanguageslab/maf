package maf.analysis.store

import maf.util.Address
import maf.util.HMapKey
import cats.Show

trait StoreAddress extends Address, HMapKey
object StoreAddress:
    type Aux[V] = StoreAddress { type Value = V }
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
trait StringAddress[V] extends StoreAddress:
    type Value = V
object StringAddress:
    given [V]: Show[StringAddress[V]] with
        def show(v: StringAddress[V]): String = v.toString
trait ValAddress[V] extends StoreAddress:
    type Value = V
trait VarAddress[V] extends ValAddress[V]:
    type Value = V
