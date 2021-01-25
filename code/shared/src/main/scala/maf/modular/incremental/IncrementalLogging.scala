package maf.modular.incremental

import maf.core.Expression
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

  // Starging the incremental analysis.
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
    } catch {
      case t: Throwable =>
        logger.logException(t)
        throw t
    }
  }

  /*
  override def updateAddrInc(
      cmp: Component,
      addr: Addr,
      nw: Value
    ): Boolean = {
    val b = super.updateAddrInc(cmp, addr, nw)
    logger.log(s"J $addr <<= ${inter.store.getOrElse(addr, lattice.bottom)} (W $nw)")
    b
  }
   */

  trait IncrementalLoggingIntra extends IncrementalGlobalStoreIntraAnalysis {
    intra =>

    // Analysis of a component.
    abstract override def analyze(timeout: Timeout.T): Unit = {
      logger.log(s"Analysing $component")
      //if (tarjanFlag) logger.log(s"* S Resetting addressDependencies for $component.")
      super.analyze()
    }

    // Reading an address.
    override def readAddr(addr: Addr): Value = {
      val v = super.readAddr(addr)
      //logger.log(s"R $addr => $v")
      v
    }

    // Writing an address.
    override def writeAddr(addr: Addr, value: Value): Boolean = {
      //if (tarjanFlag) reads.foreach(r => logger.log(s"* D $r -> $addr ($component)"))
      val b = super.writeAddr(addr, value)
      //if (b) logger.log(s"W $addr <= $value (becomes ${intra.store.getOrElse(addr, lattice.bottom)})")
      b
    }

    // Registering of provenances.
    //override def registerProvenances(): Unit = {
    //  intraProvenance.foreach({ case (addr, value) => logger.log(s"P $addr: $value") })
    // super.registerProvenances()
    //}

  }

}
