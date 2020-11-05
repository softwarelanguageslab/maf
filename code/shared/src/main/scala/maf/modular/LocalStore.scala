package maf.modular

import maf.core.{Expression, Lattice}
import maf.lattice.WrappedLattice
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
  import maf.lattice.MapLattice._
  import TaggedMap._

  type Store = Map[Addr, Value]
  type T     = TaggedMap[Addr, Value]

  implicit def dataLattice: Lattice[TaggedMap[Addr, Value]] =
    WrappedLattice.wrappedLattice(mapLattice[Addr, Value](lattice), taggedMapWrapper)

  def viewStore(c: Component): Store
  def newComponentWithStore(c: Component, store: Store): Component =
    instrument(c, TaggedMap(store))

  override def intraAnalysis(component: Component): LocalStoreIntra

  trait LocalStoreIntra extends IntraAnalysis with InstrumentationIntraAnalsyis {
    def callLocal(cmp: Component, store: Store): (Value, Store) = {
      val value        = callInstrumented(cmp, TaggedMap(store))
      val updatedStore = readReturnStore(cmp)
      (value, updatedStore)
    }

    def writeReturnStore(store: Store): Unit = {
      writeInstrumentationData(TaggedMap(store))
    }

    def readReturnStore(component: Component): Store = {
      readInstrumentationData(component).v
    }

    def componentStore: Store = viewStore(component)
  }
}
