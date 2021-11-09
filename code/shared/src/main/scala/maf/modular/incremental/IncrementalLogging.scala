package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.*
import maf.util.Logger
import maf.util.Logger.*
import maf.util.benchmarks.*
import maf.util.graph.DotGraph

/**
 * Provides facilities for logging an incremental analysis that uses the incremental global store.
 *
 * @tparam Expr
 *   The type of the expressions under analysis.
 */
trait IncrementalLogging[Expr <: Expression] extends IncrementalGlobalStore[Expr]:
    inter =>

    val logger: NumberedLog = Logger.numbered()
    var table: Table[String] = Table.empty.withDefaultValue("")
    var step: Int = 0 // The current intra-component analysis.
    var botRead: Option[Addr] = None // The analysis of a component (sometimes) stops when reading bottom. Keep whether bottom was the last read value, and if so, the corresponding address read.
    var repeats: Map[Component, Integer] = Map.empty.withDefaultValue(0) // Keep track of how many times every component has been analysed.

    def focus(a: Addr): Boolean = false // Whether to "watch"" an address and insert it into the table.

    private def legend(): String =
      """***** LEGEND OF ABBREVIATIONS *****
      |ADEP  An address value dependency is registered. Includes the "source" address and the address where the value flows to.
      |ADP*  Similar to ADEP, but the address dependency originates from an implicit value flow (e.g., due to a conditional).
      |ANLY  Analysis of a component, indicating the step of the analysis and the number of times the current component is now analysed.
      |CINV  Component invalidation: the given component is deleted.
      |COMI  Indicates the component's analysis is committed.
      |DINV  Dependency invalidation: the component is no longer dependent on the dependency.
      |IUPD  Incremental update of the given address, indicating the value now residing in the store and the value actually written.
      |PROV  Registration of provenance, including the address and new provenance value, for values that did not cause store changes.
      |READ  Address read, includes the address and value retrieved from the store.
      |RSAD  Indicates the address dependencies for a given component are reset.
      |TRIG  Indicates the given dependency has been triggered.
      |WRIT  Address write, including the address, value written and value now residing in the store.
      |
      |***** ANALYSIS SUMMARY TABLE *****
      |The column "bot" indicates whether bottom was read from the store (including the address), and hence whether the analysis of a component was prematurely terminated.
      |""".stripMargin
    logger.logU(legend())

    private def insertTable(messageOrComponent: Either[String, Component]): Unit =
        val stepString = step.toString
        step = step + 1
        messageOrComponent match
            case Left(msg) =>
              table = table.add(stepString, "Phase", msg)
            case Right(cmp) =>
              val addrs = store.keySet
              table = table.add(stepString, "Phase", cmp.toString)
              addrs.foreach(addr =>
                if focus(addr) then {
                  val v = store.getOrElse(addr, lattice.bottom)
                  val p = provenance(addr).map({ case (c, v) => s"$v ($c)" }).mkString("; ")
                  table = table.add(stepString, s"σ($addr)", v.toString).add(stepString, s"P($addr)", p)
                }
              )
              dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }).foreach { case (addr, valueSources) =>
                if focus(addr) then table = table.add(stepString, s"~> $addr", valueSources.mkString(";")) // Show the addresses on which the value at addr depends.
              }
              botRead.foreach(addr => table = table.add(stepString, "Bot", addr.toString))
              botRead = None

    private def tableToString(): String =
        val storeCols = table.allColumns.filter(_.startsWith("σ")).toList.sorted
        val provCols = table.allColumns.filter(_.startsWith("P(")).toList.sorted
        val depCols = table.allColumns.filter(_.startsWith("~>")).toList.sorted
        table.prettyString(rowName = "Step",
                           rows = (0 until step).toList.map(_.toString),
                           columns = "Phase" :: storeCols ::: provCols ::: depCols ::: List("Bot")
        )

    private def addressDependenciesToString(): String =
        // Map[W, Set[R]]
        val deps = dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) })
        val depString = deps
          .foldLeft(Table.empty.withDefaultValue(""))({ case (table, dep) => table.add(dep._1.toString, "Value sources", dep._2.mkString("; ")) })
          .prettyString()
        val scaString = computeSCAs().map(_.mkString("{", ", ", "}")).mkString("\n")
        depString + "\nSCAs:\n" + (if (scaString.isEmpty) then "none" else scaString)

    private def programToString(): String =
        val str = program.asInstanceOf[SchemeExp].prettyString()
        "Desugared program:\n\n" + str

    private def logData(end: Boolean): Unit =
        if end then logger.logU("\n\n" + getSummary())
        logger.logU("\n\n" + tableToString())
        logger.logU("\n" + storeString())
        logger.logU("\n" + addressDependenciesToString())
        if end then logger.logU("\n\n" + programToString())

    // Collect some numbers
    private var intraC: Long = 0
    private var intraCU: Long = 0
    private var deletedA: List[Addr] = Nil
    private var deletedC: List[Component] = Nil

    def getSummary(): String =
      configuration.infoString() + "\n" +
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
         |      * ${deletedC.toSet[Component].map[(Component, Int)]({ (c: Component) => (c, deletedC.count(_ == c)) }).toString()}
         |  - deleted Addresses:  ${deletedA.size} (might have been recreated later)
         |      * distinct addresses: ${deletedA.toSet.size}
         |      * deleted addresses in final store: ${deletedA.toSet.count(store.keySet)}
         |      * ${deletedA.toSet[Addr].map[(Addr, Int)]({ (a: Addr) => (a, deletedA.count(_ == a)) }).toString()}
         |##########################################""".stripMargin

    // Avoids logging the store twice from `updateAnalysis`.
    var logEnd = true

    override def run(timeout: Timeout.T): Unit =
        super.run(timeout)
        if logEnd then logData(false)

    // Starting the incremental analysis.
    override def updateAnalysis(timeout: Timeout.T): Unit =
        logger.resetNumbering()
        logger.logU("\nUpdating analysis\n")
        insertTable(Left("Updating analysis"))
        try
            logEnd = false
            super.updateAnalysis(timeout)
            logEnd = true
            //if (configuration.cyclicValueInvalidation) {
            //  addressDependencies.foreach({ case (cmp, map) =>
            //    map.foreach { case (a, addr) => logger.log(s"$a depends on $addr ($cmp)") }
            //  })
            //}
            logData(true)
        catch
            case t: Throwable =>
              logger.logException(t)
              logData(true)
              throw t

    override def deregister(target: Component, dep: Dependency): Unit =
        logger.log(s"DINV $target <-\\- $dep")
        super.deregister(target, dep)

    override def deleteComponent(cmp: Component): Unit =
        logger.log(s"CINV $cmp")
        deletedC = cmp :: deletedC
        super.deleteComponent(cmp)

    override def deleteAddress(addr: Addr): Unit =
        deletedA = addr :: deletedA
        super.deleteAddress(addr)

    override def trigger(dep: Dependency): Unit =
        logger.log(s"TRIG $dep [adding: ${deps(dep)}]")
        super.trigger(dep)

    override def updateAddrInc(cmp: Component, addr: Addr, nw: Value): Boolean =
        val b = super.updateAddrInc(cmp, addr, nw)
        logger.log(s"IUPD $addr <<= ${inter.store.getOrElse(addr, lattice.bottom)} (W $nw)")
        b

    trait IncrementalLoggingIntra extends IncrementalGlobalStoreIntraAnalysis:
        intra =>

        // Analysis of a component.
        abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit =
            logger.logU("") // Adds a newline to the log.
            if version == Old then intraC += 1 else intraCU += 1
            repeats = repeats + (component -> (repeats(component) + 1))
            logger.log(s"ANLY $component (analysis step $step, analysis # of this component: ${repeats(component)})")
            if configuration.cyclicValueInvalidation then logger.log(s"RSAD Resetting addressDependencies for $component.")
            super.analyzeWithTimeout(timeout)

        // Reading an address.
        override def readAddr(addr: Addr): Value =
            val v = super.readAddr(addr)
            if v == lattice.bottom then botRead = Some(addr) else botRead = None
            logger.log(s"READ $addr => $v")
            v

        // Writing an address.
        override def writeAddr(addr: Addr, value: Value): Boolean =
            if configuration.cyclicValueInvalidation then
                lattice.getAddresses(value).foreach(r => logger.log(s"ADEP $r ~> $addr"))
                implicitFlows.flatten.foreach(f => logger.log(s"ADP* $f ~> $addr"))
            val b = super.writeAddr(addr, value)
            logger.log(s"WRIT $value => $addr (${if b then "becomes" else "remains"} ${intra.store.getOrElse(addr, lattice.bottom)})")
            b

        // Incremental store update.
        override def doWriteIncremental(): Unit =
            intraProvenance.foreach({ case (addr, value) => logger.log(s"PROV $addr: $value") })
            super.doWriteIncremental()

        override def commit(): Unit =
            logger.log("COMI")
            super.commit()
            insertTable(Right(component))

    end IncrementalLoggingIntra
