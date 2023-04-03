package maf.analysis.store

import maf.util.*

trait StoreAddress extends HMapKey, Address
trait VectorAddress[V] extends StoreAddress:
    type Value = V
trait PairAddress[V] extends StoreAddress:
    type Value = V
