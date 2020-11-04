package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core._
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.scheme.AnalysisBuilder._
import maf.modular.scheme._
import maf.util.{Formatter, Reader}
import maf.util.Writer._
import maf.util.benchmarks._
import scala.concurrent.duration._

trait IncrementalPrecision[E <: Expression] extends IncrementalExperiment[E] {

  final val eq: String = "Equal"        // Precision of incremental update equals the one of a full reanalysis.
  final val mp: String = "More precise" // Precision of incremental update is better than the one of a full reanalysis.
  final val lp: String = "Less precise" // Precision of incremental update is lower than the one of a full reanalysis.

  var results: Table[String] = Table.empty

  def onBenchmark(file: String): Unit = {
    println(s"Testing $file")
    val program = parse(file)

    // Initial analysis: analyse + update.
    val a1 = analysis(program)

    // Base case: analysis of new program version.
    val a2 = analysis(program)
    a2.version = New

    var timeOut: Timeout.T = Timeout.none

    // Run the initial analysis.
    print(s"init ")
    timeOut = timeout()
    a1.analyze(timeOut)
    if (timeOut.reached) { // We do not use the test `a1.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.
      println("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }

    // Update the initial analysis.
    print(s"-> incr ")
    timeOut = timeout()
    a1.updateAnalysis(timeOut)
    if (timeOut.reached) {
      println("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }

    // Run a full reanalysis
    print(s"-> rean ")
    timeOut = timeout()
    a2.analyze(timeOut)
    if (timeOut.reached) {
      println("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }

    // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
    val iStore = a1.store.withDefaultValue(a1.lattice.bottom)
    val rStore = a2.store.withDefaultValue(a2.lattice.bottom)

    val allAddr = iStore.keySet.filter(interestingAddress) ++ rStore.keySet.filter(interestingAddress)
    var e: Long = 0L
    var l: Long = 0L
    var m: Long = 0L
    val t: Long = allAddr.size.toLong
    allAddr.foreach({ a =>
      val incr = iStore(a)
      val rean = rStore(a)
      if (incr == rean)
        e += 1 // Both results are the same => equally precise.
      else if (a1.lattice.subsumes(incr, rean.asInstanceOf[a1.Value]))
        l += 1 // The incremental value subsumes the value of the full reanalysis => less precise.
      else
        m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
    })
    results = results.add(file, eq, Formatter.withPercent(e, t)).add(file, mp, Formatter.withPercent(m, t)).add(file, lp, Formatter.withPercent(l, t))
  }

  def interestingAddress[A <: Address](a: A): Boolean
  def reportError(file: String): Unit = results = results.add(file, eq, "E").add(file, mp, "E").add(file, lp, "E")
  def createOutput(): String = results.prettyString(columns = List(eq, lp, mp))
}

trait IncrementalSchemePrecision extends IncrementalPrecision[SchemeExp] {
  override def interestingAddress[A <: Address](a: A): Boolean = a match {
    case PrmAddr(_) => false
    case _ => true
  }
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModFPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.scam2020ModF
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  val outputFile: String = s"precision/modf.txt"
}

object IncrementalSchemeModConcPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.scam2020ModConc
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  val outputFile: String = s"precision/modconc.txt"
}

object IncrementalSchemeModXPrecision {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModFPrecision.main(args)
    IncrementalSchemeModConcPrecision.main(args)
  }
}
