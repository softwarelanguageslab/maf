package maf.modular.incremental

import maf.core.Expression
import maf.util.Logger
import maf.util.Logger.Log
import maf.util.benchmarks.Timeout

/**
 * Provides facilities for logging an incremental analysis that uses the incremental global store.
 *
 * @tparam Expr The type of the expressions under analysis.
 */
trait IncrementalLogging[Expr <: Expression] extends IncrementalGlobalStore[Expr] {
  inter =>

  val logger: Log = Logger()

  private var count: Int = 0

  def c(s: String): String = {
    count += 1
    s"${toWidth(count.toString + ".", 4)} $s"
  }

  private def toWidth(s: String, w: Int): String = s + (" " * (w - s.length))

  override def updateAnalysis(timeout: Timeout.T, optimisedExecution: Boolean): Unit = {
    count = 0
    logger.log("\nUpdating analysis\n")
    super.updateAnalysis(timeout, optimisedExecution)
  }

  override def updateAddrInc(
      cmp: Component,
      addr: Addr,
      nw: Value
    ): Boolean = {
    val b = super.updateAddrInc(cmp, addr, nw)
    logger.log(c(s"J $addr <<= ${inter.store.getOrElse(addr, lattice.bottom)} (W $nw)"))
    b
  }

  trait IncrementalLoggingIntra extends IncrementalGlobalStoreIntraAnalysis {
    intra =>

    abstract override def analyze(timeout: Timeout.T): Unit = {
      logger.log(c(s"Analysing $component"))
      super.analyze()
    }

    override def readAddr(addr: Addr): Value = {
      val v = super.readAddr(addr)
      logger.log(c(s"R $addr => $v"))
      v
    }

    override def writeAddr(addr: Addr, value: Value): Boolean = {
      val b = super.writeAddr(addr, value)
      if (b) logger.log(c(s"W $addr <= $value (becomes ${intra.store.getOrElse(addr, lattice.bottom)})"))
      b
    }

    override def registerProvenances(): Unit = {
      intraProvenance.foreach({ case (addr, value) => logger.log(c(s"P $addr: $value")) })
      super.registerProvenances()
    }

  }

}
