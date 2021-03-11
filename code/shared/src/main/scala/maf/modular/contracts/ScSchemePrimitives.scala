package maf.modular.contracts

import maf.core.{Environment, Identity}
import maf.language.contracts.{ScExp}
import maf.language.contracts.ScLattice.{Arr, Grd, Thunk}
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
      //"+" -> ("number?" ~> "number?" ~> "number?"),
      "-" -> ("number?" ~> "number?" ~> "number?"),
      "*" -> ("number?" ~> "number?" ~> "number?"),
      "/" -> ("number?" ~> "nonzero?" ~> "number?"),
      "=" -> ("number?" ~> "number?" ~> "bool?"),
      "number?" -> ("any?" ~> "bool?"),
      //"procedure?" -> ("any?" ~> "bool?"),
      "bool?" -> ("any?" ~> "bool?"),
      //">" -> ("number?" ~> "number?" ~> "bool?"),
      "<" -> ("number?" ~> "number?" ~> "bool?"),
      //"=<" -> ("number?" ~> "number?" ~> "bool?"),
      "dependent-contract?" -> ("any?" ~> "bool?"),
      //"any?" -> ("any?" ~> "bool?"),
      // "and" -> ("any?" ~> "any?" ~> "any?"),
      //"or" -> ("any?" ~> "any?" ~> "any?"),
      "nonzero?" -> ("number?" ~> "bool?"),
      "pair?" -> ("any?" ~> "bool?"),
      "number?" -> ("any?" ~> "bool?"),
      //"not" -> ("any?" ~> "bool?"),
      "char?" -> ("any?" ~> "bool?"),
      "vector?" -> ("any?" ~> "bool?"),
      "string?" -> ("any?" ~> "bool?"),
      "string-length" -> ("string?" ~> "number?"),
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
      println(s"looking up ${name}")
      store += primAddr -> lattice.schemeLattice.primitive(name)
      store += ScMonitoredPrimAddr(name) -> lattice.arr(
        Arr(Identity.none, Identity.none, contractAddr, primAddr)
      )
    }

  private lazy val otherPrimitives =
    primMap.keys.toSet -- primitives.map(_._1)

  /** Inject the other scheme primitives that do not have a contract (yet) */
  def setupOtherPrimitives(): Unit =
    otherPrimitives.foreach { name =>
      store += ScPrimAddr(name) -> lattice.schemeLattice.primitive(name)
    }

  def primBindings: Iterable[(String, Addr)] =
    primitives.keys.map(name => (name, ScMonitoredPrimAddr(name))) ++
      otherPrimitives.map(name => (name, ScPrimAddr(name)))

  def baseEnv: Env = Environment(primBindings)
  def setup(): Unit = {
    println("Setting up analysis")
    setupMonitoredPrimitives()
    setupOtherPrimitives()
  }
}
