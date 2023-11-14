package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.*
import maf.util.ColouredFormatting.*
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
trait IncrementalLogging[Expr <: Expression] extends IncrementalDataFlowVisualisation[Expr]:
    inter =>

    val logger: NumberedLog = Logger.numbered()
    var table: Table[String] = Table.empty.withDefaultValue("")
    var step: Int = 1 // The current intra-component analysis.
    var botRead: Option[Addr] = None // The analysis of a component (sometimes) stops when reading bottom. Keep whether bottom was the last read value, and if so, the corresponding address read.
    var repeats: Map[Component, Integer] = Map.empty.withDefaultValue(0) // Keep track of how many times every component has been analysed.

    def focus(a: Addr): Boolean = false // Whether to "watch" an address and insert it into the table.

    var maxLen: Int = 50
    def crop(string: String, length: Int = maxLen): String = if string.length <= length then string else string.take(length) + "..."

    /**
     * Modes for logging:
     * * Fine: most fine-grained logging, logs everything
     * * Select: logs everything related to selected addresses (specified by overriding the select function) and non-address related matters
     * * Coarse: more coarse-grained logging, logs main analysis events
     * * Step: logs specific analysis steps (i.e., intra-component analyses) of the analysis (specified by the stepFocus variable)
     * * Summary: only outputs a summary of the analysis, showing which components have been analysed, the store, etc.
     */
    enum Mode:
        case Fine, Select, Coarse, Step, Summary

    import Mode.*
    var mode: Mode = Fine
    var stepFocus: Set[Int] = Set() // to set when Step Mode is used

    def enable(): Unit = logger.enable()
    def disable(): Unit = logger.disable()

    private def select(addr: Addr): Boolean = mode == Select && focus(addr)
    private def stepSelect(): Boolean = mode == Step && stepFocus.contains(step)

    private def legend(): String =
        """***** LEGEND OF ABBREVIATIONS *****
      |ADEP  An address value dependency is registered. Includes the "source" address and the address where the value flows to.
      |ADP*  Similar to ADEP, but the address dependency originates from an implicit value flow (e.g., due to a conditional).
      |ANLY  Analysis of a component, indicating the step of the analysis and the number of times the current component is now analysed.
      |CINV  Component invalidation: the given component is deleted.
      |COMI  Indicates the component's analysis is committed.
      |DELA  Indicates the removal of a given address.
      |DINV  Dependency invalidation: the component is no longer dependent on the dependency.
      |ICFL  The cut inter-component implicit flows to every component, found by the currently analysed component.
      |IUPD  Incremental update of the given address, indicating the value now residing in the store and the value actually written.
      |NEWC  Discovery of a new, not yet existing component.
      |PROV  Registration of provenance, including the address and new provenance value, for values that did not cause store changes.
      |READ  Address read, includes the address and value retrieved from the store.
      |RSAD  Indicates the address dependencies for a given component are reset.
      |RSCA  Indicates (an address in) a SCA is refined. The number indicates the number of SCA refinement.
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
                        val p = provenance(addr).map({ case (c, v) => s"${crop(v.toString)} ($c)" }).mkString("; ")
                        table = table.add(stepString, s"σ(${crop(addr.toString)})", crop(v.toString)).add(stepString, s"P(${crop(addr.toString)}", p)
                    }
                )
                dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }).foreach { case (addr, valueSources) =>
                    if focus(addr) then table = table.add(stepString, s"~> $addr", valueSources.mkString(";")) // Show the addresses on which the value at addr depends.
                }
                botRead.foreach(addr => table = table.add(stepString, "Bot", crop(addr.toString)))
                botRead = None

    private def tableToString(): String =
        val storeCols = table.allColumns.filter(_.startsWith("σ")).toList.sorted
        val provCols = table.allColumns.filter(_.startsWith("P(")).toList.sorted
        val depCols = table.allColumns.filter(_.startsWith("~>")).toList.sorted
        table.prettyString(rowName = "Step",
                           rows = (1 until step).toList.map(_.toString),
                           columns = "Phase" :: storeCols ::: List("Bot"),
                           format = s => crop(s)
        ) + "\n\n" +
        table.prettyString(rowName = "Step",
            rows = (1 until step).toList.map(_.toString),
            columns = "Phase" :: provCols,
            format = s => crop(s)
        ) + "\n\n" +
        table.prettyString(rowName = "Step",
            rows = (1 until step).toList.map(_.toString),
            columns = "Phase" :: depCols,
            format = s => crop(s)
        )

    private def addressDependenciesToString(): String =
        // Map[W, Set[R]]
        val deps = dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) })
        val depString = deps
            .foldLeft(Table.empty.withDefaultValue(""))({ case (table, dep) => table.add(dep._1.toString, "Value sources", dep._2.mkString("; ")) })
            .prettyString()
        // Inter-component dataflow
        val icFlow = interComponentFlow.values.flatten.groupBy(_._1).map({case (c, ca) => (c, ca.flatMap(_._2).toSet)})
        val icFlowString = icFlow
            .foldLeft(Table.empty.withDefaultValue(""))( {case (table, flow) => table.add(flow._1.toString, "Flows to cmp", flow._2.mkString("; "))})
            .prettyString()
        val scaString = computeSCAs().map(_.map(a => crop(a.toString)).mkString("{", ", ", "}")).mkString("\n")
        depString + "\n\n" + icFlowString + (if configuration.cyclicValueInvalidation && mode != Select then "\nSCAs:\n" + (if scaString.isEmpty then "none" else scaString)
                     else "")

    private def programToString(): String =
        if mode != Select then "Desugared program:\n\n" + program.asInstanceOf[SchemeExp].prettyString() else ""

    private def logData(end: Boolean): Unit =
        if end then logger.logU("\n\n" + getSummary())
        logger.logU("\n\n" + tableToString())
        if mode == Fine || mode == Coarse || mode == Step then logger.logU("\n" + storeString().split("\n").nn.mkString("\n"))
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
        logger.logU(configString())
        try super.run(timeout)
        catch
            case t: Throwable =>
                logger.logException(t)
                logData(true)
                throw t
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
            logData(true)
        catch
            case t: Throwable =>
                logger.logException(t)
                logData(true)
                throw t

    override def deregister(target: Component, dep: Dependency): Unit =
        if mode == Fine || mode == Select || stepSelect() then logger.log(s"DINV $target <-\\- $dep")
        super.deregister(target, dep)

    override def deleteComponent(cmp: Component): Unit =
        if mode == Fine || mode == Select || stepSelect() then logger.log(s"CINV $cmp")
        deletedC = cmp :: deletedC
        super.deleteComponent(cmp)

    override def deleteAddress(addr: Addr): Unit =
        if mode == Fine || select(addr) || stepSelect() then logger.log(s"DELA $addr")
        deletedA = addr :: deletedA
        super.deleteAddress(addr)

    override def trigger(dep: Dependency): Unit =
        if mode == Fine || mode == Coarse || stepSelect() then logger.log(s"TRIG ${crop(dep.toString)} [adding: ${crop(deps(dep).toString())}]")
        super.trigger(dep)

    override def updateAddrInc(cmp: Component, addr: Addr, nw: Value): Boolean =
        val b = super.updateAddrInc(cmp, addr, nw)
        if mode == Fine || select(addr) || stepSelect() then
            logger.log(s"IUPD ${crop(addr.toString)} <<= ${crop(inter.store.getOrElse(addr, lattice.bottom).toString)} (W ${crop(nw.toString)})")
        b

    override def spawn(cmp: Component): Unit =
        if (mode != Summary && (mode != Step || stepSelect())) && !visited(cmp) then logger.log(s"NEWC ${crop(cmp.toString)}")
        super.spawn(cmp)

    private var rSCAcount = 0
    override def refineSCA(sca: SCA): Unit =
        rSCAcount = rSCAcount + 1
        dataFlowToImage("pre-RSCA.dot")
        var values: Map[Addr, Value] = Map()
        if mode != Step || stepSelect() then sca.foreach(a => values = values + (a -> store.getOrElse(a, lattice.bottom)))
        super.refineSCA(sca)
        if mode != Step || stepSelect() then
            sca.toList.sortBy(_.toString).map(a => s"$a (${values(a)} -> ${store.getOrElse(a, lattice.bottom)})").foreach(u => logger.log(s"RSCA ($rSCAcount) $u"))

    trait IncrementalLoggingIntra extends IncrementalGlobalStoreCYIntraAnalysis:
        intra =>
        // Analysis of a component.
        abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit =
            //if rSCAcount >= 10
            //then
            //    logger.logU(s"RSCA count = 2, aborting analysis before step $step")
            //    throw new Exception("Aborting analysis!")
            if mode != Summary && (mode != Step || stepSelect()) then logger.logU("") // Adds a newline to the log.
            if version == Old then intraC += 1 else intraCU += 1
            repeats = repeats + (component -> (repeats(component) + 1))
            if mode != Step || stepSelect() then
                logger.log(s"ANLY $component (analysis step $step, analysis # of this component: ${repeats(component)})")
            if configuration.cyclicValueInvalidation && (mode == Fine || stepSelect()) then
                logger.log(s"RSAD Resetting addressDependencies for $component.")
            super.analyzeWithTimeout(timeout)

        // Reading an address.
        override def readAddr(addr: Addr): Value =
            val v = super.readAddr(addr)
            if lattice.isBottom(v) then botRead = Some(addr) else botRead = None
            if mode == Fine || select(addr) || stepSelect() then logger.log(s"READ ${crop(addr.toString)} => ${crop(v.toString)}")
            v

        // Writing an address.
        override def writeAddr(addr: Addr, value: Value): Boolean =
            if configuration.cyclicValueInvalidation && mode != Summary && (mode != Step || stepSelect()) then
                lattice
                    .getAddresses(value)
                    .foreach(r =>
                        if (mode != Select || focus(addr)) && (mode != Step || stepSelect()) then
                            logger.log(s"ADEP ${crop(r.toString)} ~> ${crop(addr.toString)}")
                    )
                //implicitFlows.flatten.foreach(f =>
                //    if (mode != Select || focus(addr)) && (mode != Step || stepSelect()) then
                //        logger.log(s"ADP* ${crop(f.toString)} ~> ${crop(addr.toString)}")
                //)
            val b = super.writeAddr(addr, value)
            if mode == Fine || select(addr) || stepSelect() then
                logger.log(
                  s"WRIT ${crop(value.toString)} => ${crop(addr.toString)} (${if b then "becomes" else "remains"} ${crop(intra.store.getOrElse(addr, lattice.bottom).toString)})"
                )
            b

        // Incremental store update.
        override def doWriteIncremental(): Unit =
            if mode != Summary && (mode != Step || stepSelect()) then
                intraProvenance.foreach({ case (addr, value) =>
                    if (mode != Select || focus(addr)) && (mode != Step || stepSelect()) then
                        logger.log(s"PROV ${crop(addr.toString)}: ${crop(value.toString)}")
                })
            super.doWriteIncremental()

        override def commit(): Unit =
            if mode != Summary || stepSelect() then logger.log("COMI")
            super.commit()
            if (mode == Fine || (mode == Step && stepSelect())) then logger.log(s"ICFL ${interComponentFlow.getOrElse(component, Map()).toList.map(kv => s"${kv._1} => {${kv._2.mkString(", ")}}").mkString("; ")}")
            insertTable(Right(component))

    end IncrementalLoggingIntra
