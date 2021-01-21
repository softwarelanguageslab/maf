package maf.modular.contracts

import maf.core.{Environment, Identity}
import maf.language.contracts.{ScExp, ScLattice}
import maf.language.contracts.ScLattice.{Arr, Grd, Prim, Thunk}
import maf.modular.GlobalStore

trait ScSchemePrimitives extends ScModSemanticsScheme with GlobalStore[ScExp] {
  trait Implies {
    def ~>(implies: Implies): Implies

    def collectDomainContracts(
        implies: Implies
      ): List[ScAddresses[Addr]] = implies match {
      case StringImplication(prim) => List(ScPrimAddr(prim))
      case Implication(left, right) =>
        collectDomainContracts(left) ++ collectDomainContracts(right)
    }

    def asGrd(name: String): Value = this match {
      case Implication(left, StringImplication(range)) =>
        val rangeMaker = lattice.thunk(Thunk(ScPrimAddr(range)))
        val rangeMakerAddr = ScPrimRangeAddr(name)

        store += rangeMakerAddr -> rangeMaker
        lattice.grd(Grd(collectDomainContracts(left), rangeMakerAddr))

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
      "+" -> ("int?" ~> "int?" ~> "int?"),
      "-" -> ("int?" ~> "int?" ~> "int?"),
      "*" -> ("int?" ~> "int?" ~> "int?"),
      "/" -> ("int?" ~> "nonzero?" ~> "int?"),
      "=" -> ("int?" ~> "int?" ~> "bool?"),
      "int?" -> ("any?" ~> "bool?"),
      "proc?" -> ("any?" ~> "bool?"),
      "bool?" -> ("any?" ~> "bool?"),
      ">" -> ("int?" ~> "int?" ~> "bool?"),
      "<" -> ("int?" ~> "int?" ~> "bool?"),
      "=<" -> ("int?" ~> "int?" ~> "bool?"),
      "dependent-contract?" -> ("any?" ~> "bool?"),
      "any?" -> ("any?" ~> "bool?"),
      "and" -> ("any?" ~> "any?" ~> "any?"),
      "or" -> ("any?" ~> "any?" ~> "any?"),
      "nonzero?" -> ("int?" ~> "bool?"),
      "pair?" -> ("any?" ~> "bool?"),
      "number?" -> ("any?" ~> "bool?"),
      "not" -> ("any?" ~> "bool?"),
      "char?" -> ("any?" ~> "bool?"),
      "vector?" -> ("any?" ~> "bool?"),
      "string?" -> ("any?" ~> "bool?"),
      "string-length" -> ("string?" ~> "int?"),
      "symbol?" -> ("any?" ~> "bool?"),
      "true?" -> ("any?" ~> "bool?"),
      "false?" -> ("any?" ~> "bool?"),
      "null?" -> ("any?" ~> "any?"),
      "cons" -> ("any?" ~> "any?" ~> "pair?"),
      "car" -> ("pair?" ~> "any?"),
      "cdr" -> ("pair?" ~> "any?")
    )

  /** Wraps the primitives into monitors */
  def setupMonitoredPrimitives(): Unit =
    primitives.foreach { case (name, implies) =>
      val contractAddr = ScGrdAddr(name)
      val primAddr = ScPrimAddr(name)
      val grd = implies.asGrd(name)
      store += contractAddr -> grd
      store += primAddr -> lattice.schemeLattice.primitive(primMap(name))
      store += ScMonitoredPrimAddr(name) -> lattice.arr(
        Arr(Identity.none, Identity.none, contractAddr, primAddr)
      )
    }

  def primBindings: Iterable[(String, Addr)] =
    primitives.keys.map(name => (name, ScMonitoredPrimAddr(name)))

  def baseEnv: Env = Environment(primBindings)
  def setup(): Unit = {
    println("Setting up analysis")
    setupMonitoredPrimitives()
  }
}
