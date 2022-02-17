package maf.modular
import maf.core.Expression
import maf.util.Logger
import maf.util.Logger._
import maf.util.benchmarks.{Table, Timeout}

/**
 * Provides facilities for logging an incremental analysis that uses the incremental global store.
 *
 * @tparam Expr
 *   The type of the expressions under analysis.
 */
trait AnalysisLogging[Expr <: Expression] extends GlobalStore[Expr]:
    inter =>

    val logger: NumberedLog = Logger.numbered()
    var table: Table[String] = Table.empty.withDefaultValue("")
    var step: Int = 0 // The current intra-component analysis.
    var botRead: Option[Addr] =
      None // The analysis of a component (sometimes) stops when reading bottom. Keep whether bottom was the last read value, and if so, the corresponding address read.
    var repeats: Map[Component, Integer] = Map.empty.withDefaultValue(0) // Keep track of how many times every component has been analysed.

    def focus(a: Addr): Boolean = !a.toString.contains("Prm") // Whether to "watch"" an address and insert it into the table.

    enum Mode:
        case Fine, Coarse //, Summary

    import Mode.*
    var mode: Mode = Fine

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
                  table = table.add(stepString, s"σ($addr)", v.toString)
                }
              )
              botRead.foreach(addr => table = table.add(stepString, "Bot", addr.toString))
              botRead = None

    private def tableToString(): String =
        val storeCols = table.allColumns.filter(_.startsWith("σ")).toList.sorted
        val provcols = table.allColumns.filter(_.startsWith("P(")).toList.sorted
        table.prettyString(rowName = "Step", rows = (0 until step).toList.map(_.toString), columns = "Phase" :: storeCols ::: provcols ::: List("Bot"))

    // Collect some numbers
    private var intraC: Long = 0

    def getSummary(): String =
      s"""##########################################
         |Analysis Summary:
         |  - components: ${visited.size}
         |      ${visited.mkString(", ")}
         |  - intra-component analyses: $intraC
         |##########################################""".stripMargin

    trait AnalysisLoggingIntra extends GlobalStoreIntra:
        intra =>

        // Analysis of a component.
        abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit =
            logger.logU("") // Adds a newline to the log.
            intraC += 1
            repeats = repeats + (component -> (repeats(component) + 1))
            if mode == Fine then logger.log(s"ANLY $component (analysis step $step, analysis # of this component: ${repeats(component)})")
            super.analyzeWithTimeout(timeout)

        // Reading an address.
        override def readAddr(addr: Addr): Value =
            val v = super.readAddr(addr)
            if v == lattice.bottom then botRead = Some(addr) else botRead = None
            if mode == Fine then logger.log(s"READ $addr => $v")
            v

        // Writing an address.
        override def writeAddr(addr: Addr, value: Value): Boolean =
            val b = super.writeAddr(addr, value)
            if b && mode == Fine then logger.log(s"WRIT $addr <= $value (becomes ${intra.store.getOrElse(addr, lattice.bottom)})")
            b

        override def commit(): Unit =
            logger.log("COMI")
            super.commit()
            insertTable(Right(component))
            if finished then
                logger.logU("\n\n" + getSummary())
                logger.logU("\n\n" + tableToString())
                logger.logU("\n" + storeString())
