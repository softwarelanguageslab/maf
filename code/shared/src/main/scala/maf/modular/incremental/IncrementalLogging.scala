package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion._
import maf.util.Logger
import maf.util.Logger._
import maf.util.benchmarks.{Table, Timeout}

/**
 * Provides facilities for logging an incremental analysis that uses the incremental global store.
 *
 * @tparam Expr The type of the expressions under analysis.
 */
trait IncrementalLogging[Expr <: Expression] extends IncrementalGlobalStore[Expr] {
  inter =>

  val logger: NumberedLog = Logger.numbered()
  var table: Table[String] = Table.empty.withDefaultValue("")
  var step: Int = 0 // The current intra-component analysis.

  def focus(a: Addr): Boolean = false // Whether to "watch"" an address and insert it into the table.

  def insertTable(messageOrComponent: Either[String, Component]): Unit = {
    logger.log("TABULATING")
    val stepString = step.toString
    step = step + 1
    messageOrComponent match {
      case Left(msg) =>
        table = table.add(stepString, "Phase", msg)
      case Right(cmp) =>
        val addrs = store.keySet
        table = table.add(stepString, "Phase", cmp.toString)
        addrs.foreach(addr =>
          if (focus(addr)) {
            val v = store.getOrElse(addr, lattice.bottom)
            val p = provenance(addr).map({ case (c, v) => s"$v ($c)" }).mkString("; ")
            table = table.add(stepString, s"σ($addr)", v.toString).add(stepString, s"P($addr)", p)
          }
        )
    }
  }

  def tableToString(): String = {
    val storeCols = table.allColumns.filter(_.startsWith("σ")).toList.sorted
    val provcols = table.allColumns.filter(_.startsWith("P(")).toList.sorted
    table.prettyString(rowName = "Step", rows = (0 until step).toList.map(_.toString), columns = "Phase" :: storeCols ::: provcols)
  }

  // Collect some numbers
  var intraC: Long = 0
  var intraCU: Long = 0
  var deletedA: List[Addr] = Nil
  var deletedC: List[Component] = Nil

  def getSummary(): String =
    configuration.toString + "\n" +
      s"""##########################################
         |Analysis Summary:
         |  - components: ${visited.size}
         |      ${visited.mkString(", ")}
         |  - intra-component analyses: 
         |      * initial: $intraC
         |      * update:  $intraCU
         |  - component deletions: ${deletedC.size}
         |      * distinct components deleted: ${deletedC.toSet.size}
         |      * deleted components in final result: ${deletedC.toSet.count(visited)}
         |      * ${deletedC.toSet[Component].map[(Component, Int)]({ c: Component => (c, deletedC.count(_ == c)) }).toString()}
         |  - deleted Addresses:  ${deletedA.size} (might have been recreated later)
         |      * distinct addresses: ${deletedA.toSet.size}
         |      * deleted addresses in final store: ${deletedA.toSet.count(store.keySet)}
         |      * ${deletedA.toSet[Addr].map[(Addr, Int)]({ a: Addr => (a, deletedA.count(_ == a)) }).toString()}
         |##########################################""".stripMargin

  // Starting the incremental analysis.
  override def updateAnalysis(timeout: Timeout.T): Unit = {
    logger.logU("\n\n" + tableToString())
    logger.logU("\n" + storeString())
    logger.resetNumbering()
    logger.logU("\nUpdating analysis\n")
    insertTable(Left("Updating analysis"))
    try {
      super.updateAnalysis(timeout)
      if (configuration.cyclicValueInvalidation) {
        addressDependencies.foreach({ case (cmp, map) =>
          map.foreach { case (a, addr) => logger.log(s"$a depends on $addr ($cmp)") }
        })
      }
      logger.logU("\n\n" + getSummary())
      logger.logU("\n\n" + tableToString())
      logger.logU("\n" + storeString())
    } catch {
      case t: Throwable =>
        logger.logException(t)
        logger.logU("\n\n" + getSummary())
        logger.logU("\n\n" + tableToString())
        logger.logU("\n" + storeString())
        throw t
    }
  }

  override def deleteComponent(cmp: Component): Unit = {
    deletedC = cmp :: deletedC
    super.deleteComponent(cmp)
  }

  override def deleteAddress(addr: Addr): Unit = {
    deletedA = addr :: deletedA
    super.deleteAddress(addr)
  }

  override def updateAddrInc(
      cmp: Component,
      addr: Addr,
      nw: Value
    ): Boolean = {
    val b = super.updateAddrInc(cmp, addr, nw)
    logger.log(s"I $addr <<= ${inter.store.getOrElse(addr, lattice.bottom)} (W $nw)")
    //    logger.log(provenance(addr).toList.map({case (c, p) => s"          * $c :: $p"}).mkString("\n"))
    b
  }

  trait IncrementalLoggingIntra extends IncrementalGlobalStoreIntraAnalysis {
    intra =>

    // Analysis of a component.
    abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit = {
      if (version == Old) intraC += 1 else intraCU += 1
      logger.log(s"Analysing $component")
      if (configuration.cyclicValueInvalidation) logger.log(s"* S Resetting addressDependencies for $component.")
      super.analyzeWithTimeout(timeout)
    }

    // Reading an address.
    override def readAddr(addr: Addr): Value = {
      val v = super.readAddr(addr)
      logger.log(s"R $addr => $v")
      v
    }

    // Writing an address.
    override def writeAddr(addr: Addr, value: Value): Boolean = {
      if (configuration.cyclicValueInvalidation) reads.foreach(r => logger.log(s"* D $addr -> $r ($component)"))
      val b = super.writeAddr(addr, value)
      if (b) logger.log(s"W $addr <= $value (becomes ${intra.store.getOrElse(addr, lattice.bottom)})")
      b
    }

    // Registering of provenances.
    override def registerProvenances(): Unit = {
      intraProvenance.foreach({ case (addr, value) => logger.log(s"P $addr: $value") })
      super.registerProvenances()
    }

    override def commit(): Unit = {
      super.commit()
      insertTable(Right(component))
    }

  }

}
