package maf.modular

import maf.core.{Expression, Lattice}
import maf.util.Wrapper

case object LOCAL_STORE_TAG extends InstrTag

case class TaggedMap[Addr, Value](v: Map[Addr, Value]) extends TaggedData {
  def tag: InstrTag = LOCAL_STORE_TAG
}

object TaggedMap {
  implicit def taggedMapWrapper[K, V]: Wrapper[Map[K, V], TaggedMap[K, V]] =
    new Wrapper[Map[K, V], TaggedMap[K, V]] {
      def wrap(v: Map[K, V]): TaggedMap[K, V]   = TaggedMap(v)
      def unwrap(v: TaggedMap[K, V]): Map[K, V] = v.v
    }
}

trait LocalStore[Expr <: Expression] extends Instrumentation[Expr] {
  type Store = Map[Addr, Value]
  type T     = TaggedMap[Addr, Value]

  implicit val dataLattice: Lattice[TaggedMap[Addr, Value]] = implicitly

  def viewStore(c: Component): Store

  override def intraAnalysis(component: Component): LocalStoreIntra

  trait LocalStoreIntra extends IntraAnalysis with InstrumentationIntraAnalsyis {
    def callLocal(cmp: Component, store: Store): Value = {
      callInstrumented(cmp, TaggedMap(store))
    }

    def writeReturnStore(store: Store): Unit = {
      writeInstrumentationData(TaggedMap(store))
    }

    def componentStore: Store = viewStore(component)
  }
}
