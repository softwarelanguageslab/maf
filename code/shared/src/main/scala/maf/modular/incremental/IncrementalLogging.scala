package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion._
import maf.util.Logger
import maf.util.Logger._
import maf.util.benchmarks.Timeout

/**
 * Provides facilities for logging an incremental analysis that uses the incremental global store.
 *
 * @tparam Expr The type of the expressions under analysis.
 */
trait IncrementalLogging[Expr <: Expression] extends IncrementalGlobalStore[Expr] {
  inter =>

  val logger: NumberedLog = Logger.numbered()

  // Collect some numbers
  var intraC: Long = 0
  var intraCU: Long = 0
  var deletedA: Set[Addr] = Set()
  var deletedC: Set[Component] = Set()

  def getSummary(): String =
    s"""##########################################
       |Analysis Summary:
       |  - mode:
       |      * optimised: $optimisationFlag
       |      * cycle opt: $tarjanFlag
       |  - components: ${visited.size}
       |      ${visited.mkString(", ")}
       |  - intra-component analyses: 
       |      * initial: $intraC
       |      * update:  $intraCU
       |  - deleted components: ${deletedC.size} (might have been recreated later)
       |      ${deletedC.mkString(", ")}
       |  - deleted Addresses:  ${deletedA.size} (might have been recreated later)
       |      ${deletedA.mkString(", ")}
       |##########################################""".stripMargin

  // Starting the incremental analysis.
  override def updateAnalysis(timeout: Timeout.T, optimisedExecution: Boolean = true): Unit = {
    logger.resetNumbering()
    logger.logU("\nUpdating analysis\n")
    try {
      super.updateAnalysis(timeout, optimisedExecution)
      if (tarjanFlag) {
        addressDependencies.foreach({ case (cmp, map) =>
          map.foreach { case (a, addr) => logger.log(s"$a depends on $addr ($cmp)") }
        })
      }
      logger.logU("\n\n" + getSummary())
      logger.logU("\n" + storeString())
    } catch {
      case t: Throwable =>
        logger.logException(t)
        logger.logU("\n\n" + getSummary())
        logger.logU("\n" + storeString())
        throw t
    }
  }

  override def deleteComponent(cmp: Component): Unit = {
    deletedC += cmp
    super.deleteComponent(cmp)
  }

  override def deleteAddress(addr: Addr): Unit = {
    deletedA += addr
    super.deleteAddress(addr)
  }

  override def updateAddrInc(
      cmp: Component,
      addr: Addr,
      nw: Value
    ): Boolean = {
    val b = super.updateAddrInc(cmp, addr, nw)
    logger.log(s"J $addr <<= ${inter.store.getOrElse(addr, lattice.bottom)} (W $nw)")
    b
  }

  trait IncrementalLoggingIntra extends IncrementalGlobalStoreIntraAnalysis {
    intra =>

    // Analysis of a component.
    abstract override def analyze(timeout: Timeout.T): Unit = {
      if (version == Old) intraC += 1 else intraCU += 1
      logger.log(s"Analysing $component")
      if (tarjanFlag) logger.log(s"* S Resetting addressDependencies for $component.")
      super.analyze(timeout)
    }

    // Reading an address.
    override def readAddr(addr: Addr): Value = {
      val v = super.readAddr(addr)
      logger.log(s"R $addr => $v")
      v
    }

    // Writing an address.
    override def writeAddr(addr: Addr, value: Value): Boolean = {
      if (tarjanFlag) reads.foreach(r => logger.log(s"* D $addr -> $r ($component)"))
      val b = super.writeAddr(addr, value)
      if (b) logger.log(s"W $addr <= $value (becomes ${intra.store.getOrElse(addr, lattice.bottom)})")
      b
    }

    // Registering of provenances.
    //override def registerProvenances(): Unit = {
    //  intraProvenance.foreach({ case (addr, value) => logger.log(s"P $addr: $value") })
    // super.registerProvenances()
    //}

  }

}
