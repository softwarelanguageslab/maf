package maf.modular

import maf.core.{Expression, Lattice}
import maf.util.SmartHash

trait InstrTag                                                             extends SmartHash
case class InstrDependency[Component](component: Component, tag: InstrTag) extends Dependency

trait TaggedData {
  def tag: InstrTag
}

/**
  * Instrumentation allows the developer of the analysis to attach arbitrary data
  * to components, and also return modified versions of this data to the caller.
  */
trait Instrumentation[Expr <: Expression] extends ModAnalysis[Expr] with ReturnValue[Expr] {
  type T <: TaggedData
  implicit val dataLattice: Lattice[T]

  var instrumentationData: Map[Component, T] = Map().withDefaultValue(dataLattice.bottom)

  def instrument(component: Component, data: T): Component

  override def intraAnalysis(component: Component): InstrumentationIntraAnalsyis
  trait InstrumentationIntraAnalsyis extends IntraAnalysis with ReturnResultIntra {
    def callInstrumented(cmp: Component, data: T): Value = {
      // register a dependency such that when the modified version is updated, the caller component
      // can be analyzed again
      register(InstrDependency(cmp, data.tag))
      call(instrument(cmp, data))
    }

    def writeInstrumentationData(data: T): Unit = {
      instrumentationData += component -> dataLattice.join(instrumentationData(component), data)
      trigger(InstrDependency(component, data.tag))
    }
  }
}
