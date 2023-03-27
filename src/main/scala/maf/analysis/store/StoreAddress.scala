package maf.analysis.store

import maf.util.*

trait StoreAddress[S, V, P] extends HMapKey
case class VecAdr[S, V, P](address: Address) extends StoreAddress[S, V, P]:
  type Value = V
case class StringAdr[S, V, P](address: Address) extends StoreAddress[S, V, P]:
  type Value = S
case class PaiAdr[S, V, P](address: Address) extends StoreAddress[S, V, P]:
  type Value = P
