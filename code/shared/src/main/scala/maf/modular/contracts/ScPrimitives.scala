package maf.modular.contracts

import maf.core.{Environment, Identity}
import maf.language.contracts.{ScExp, ScLattice}
import maf.language.contracts.ScLattice.{Arr, Grd, Prim, Thunk}
import maf.modular.GlobalStore

trait ScPrimitives extends ScModSemantics with GlobalStore[ScExp] {
  trait Implies {
    def ~>(implies: Implies): Implies

    def collectDomainContracts(
        implies: Implies
    )(implicit lattice: ScLattice[Value, Addr]): List[ScAddresses[Addr]] = implies match {
      case StringImplication(prim) => List(ScPrimAddr(prim))
      case Implication(left, right) =>
        collectDomainContracts(left) ++ collectDomainContracts(right)
    }

    def asGrd(name: String)(implicit lattice: ScLattice[Value, Addr]): Value = this match {
      case Implication(left, StringImplication(range)) =>
        val rangeMaker     = lattice.injectThunk(Thunk(ScPrimAddr(range)))
        val rangeMakerAddr = ScPrimRangeAddr(name)

        store += rangeMakerAddr -> rangeMaker
        lattice.injectGrd(Grd(collectDomainContracts(left), rangeMakerAddr))

      case _ => throw new Exception("unsupported ")
    }
  }

  case class StringImplication(s: String) extends Implies {
    def ~>(implies: Implies): Implies = Implication(this, implies)
  }

  implicit def stringImplication(s: String): StringImplication = StringImplication(s)

  case class Implication(left: Implies, right: Implies) extends Implies {
    override def ~>(implies: Implies): Implies = Implication(this, implies)
  }

  override var store: Map[Addr, Value] = Map()

  def primitives =
    Map(
      "+"                   -> ("int?" ~> "int?" ~> "int?"),
      "-"                   -> ("int?" ~> "int?" ~> "int?"),
      "*"                   -> ("int?" ~> "int?" ~> "int?"),
      "/"                   -> ("int?" ~> "nonzero?" ~> "int?"),
      "="                   -> ("int?" ~> "int?" ~> "bool?"),
      "int?"                -> ("any?" ~> "bool?"),
      "proc?"               -> ("any?" ~> "bool?"),
      "bool?"               -> ("any?" ~> "bool?"),
      ">"                   -> ("int?" ~> "int?" ~> "bool?"),
      "<"                   -> ("int?" ~> "int?" ~> "bool?"),
      "=<"                  -> ("int?" ~> "int?" ~> "bool?"),
      "dependent-contract?" -> ("any?" ~> "bool?"),
      "any?"                -> ("any?" ~> "bool?"),
      "and"                 -> ("any?" ~> "any?" ~> "any?"),
      "nonzero?"            -> ("int?" ~> "bool?"),
      "not"                 -> ("any?" ~> "bool?") // TODO: add some tests for this
    )

  /**
    * Wraps the primitives into monitors
    */
  def setupMonitoredPrimitives(): Unit = {
    primitives.foreach {
      case (name, implies) =>
        val contractAddr = ScGrdAddr(name)
        val primAddr     = ScPrimAddr(name)
        val grd          = implies.asGrd(name)
        store += contractAddr -> grd
        store += primAddr     -> lattice.injectPrim(Prim(name))
        store += ScMonitoredPrimAddr(name) -> lattice.injectArr(
          Arr(Identity.none, Identity.none, contractAddr, primAddr)
        )
    }
  }

  def primBindings: Iterable[(String, Addr)] =
    primitives.keys.map(name => (name, ScMonitoredPrimAddr(name)))

  def baseEnv: Env = Environment(primBindings)
  def setup(): Unit = {
    setupMonitoredPrimitives()
  }
}
