package maf.modular

import maf.core.{Expression, Lattice}
import maf.lattice.WrappedLattice
import maf.util.Wrapper

case object LOCAL_STORE_TAG extends InstrTag

case class LocalStoreMap[Addr, Value](v: Map[Addr, Value]) extends TaggedData {
  def tag: InstrTag = LOCAL_STORE_TAG
}

object LocalStoreMap {
  implicit def taggedMapWrapper[K, V]: Wrapper[Map[K, V], LocalStoreMap[K, V]] =
    new Wrapper[Map[K, V], LocalStoreMap[K, V]] {
      def wrap(v: Map[K, V]): LocalStoreMap[K, V]   = LocalStoreMap(v)
      def unwrap(v: LocalStoreMap[K, V]): Map[K, V] = v.v
    }
}

trait LocalStore[Expr <: Expression] extends Instrumentation[Expr] {
  import maf.lattice.MapLattice._
  import LocalStoreMap._

  type Store = Map[Addr, Value]
  type T     = LocalStoreMap[Addr, Value]

  implicit def dataLattice: Lattice[LocalStoreMap[Addr, Value]] =
    WrappedLattice.wrappedLattice(mapLattice[Addr, Value](lattice), taggedMapWrapper)

  def viewStore(c: Component): Store
  def newComponentWithStore(c: Component, store: Store): Component =
    instrument(c, LocalStoreMap(store))

  override def intraAnalysis(component: Component): LocalStoreIntra

  trait LocalStoreIntra extends IntraAnalysis with InstrumentationIntraAnalsyis {
    def callLocal(cmp: Component, store: Store): (Value, Store) = {
      val value        = callInstrumented(cmp, LocalStoreMap(store))
      val updatedStore = readReturnStore(instrument(cmp, LocalStoreMap(store)))
      (value, updatedStore)
    }

    def writeReturnStore(store: Store): Unit = {
      writeInstrumentationData(LocalStoreMap(store))
    }

    def readReturnStore(component: Component): Store = {
      readInstrumentationData(component).v
    }

    def componentStore: Store = viewStore(component)
  }
}
