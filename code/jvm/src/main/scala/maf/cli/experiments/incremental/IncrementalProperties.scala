package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core._
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.scheme.AnalysisBuilder._
import maf.modular.scheme._
import maf.util.datastructures.ListOps.Crossable
import maf.util._
import maf.util.Writer._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalProperties[E <: Expression] extends IncrementalExperiment[E] {

  final val in: String = " (init)"        // Initial analysis
  final val up: String = " (incr)"        // Incremental update
  final val re: String = " (rean)"        // Full reanalysis

  final val al = List(in, up, re)         // Analyses

  final val co: String = "#Components"    // Number of components
  final val an: String = "#Analyses"      // Number of intra-component analyses
  final val ad: String = "|Store|"        // Store size
  final val dp: String = "#Dependencies"  // Number of dependencies

  final val pr = List(ad, co, dp, an)     // Properties

  final val inf: String = "∞"             // Timeout
  final val err: String = "E"             // Error

  var results: Table[String] = Table.empty.withDefaultValue("?")

  def onBenchmark(file: String): Unit = {
    print(s"Testing $file ")
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
      pr.foreach(p => results = results.add(file, p + in, inf))
      return
    }
    val vis = a1.visited.size
    val ssi = a1.store.keySet.size
    val dep = a1.deps.values.map(_.size).sum
    //val cnt = a1.intraCount
    results = results
      .add(file, co + in, vis.toString)
      //.add(file, an + in, cnt.toString)
      .add(file, ad + in, ssi.toString)
      .add(file, dp + in, dep.toString)

    // Update the initial analysis.
    print(s"-> incr ")
    timeOut = timeout()
    a1.updateAnalysis(timeOut)
    if (timeOut.reached) {
      println("timed out.")
      pr.foreach(p => results = results.add(file, p + up, inf))
      return
    }
    results = results
      .add(file, co + up, s"${a1.visited.size}")
      //.add(file, an + up, s"${a1.intraCount - cnt}")
      .add(file, ad + up, s"${a1.store.keySet.size}")
      .add(file, dp + up, s"${a1.deps.values.map(_.size).sum}")

    // Run a full reanalysis
    print(s"-> rean ")
    timeOut = timeout()
    a2.analyze(timeOut)
    if (timeOut.reached) {
      print("timed out.")
      pr.foreach(p => results = results.add(file, p + re, inf))
      return
    }
    results = results
      .add(file, co + re, a2.visited.size.toString)
      //.add(file, an + re, a2.intraCount.toString)
      .add(file, ad + re, a2.store.keySet.size.toString)
      .add(file, dp + re, a2.deps.values.map(_.size).sum.toString)
  }

  def interestingAddress[A <: Address](a: A): Boolean
  def reportError(file: String): Unit = pr.foreach(d => al.foreach(a => results = results.add(file, d + a, err)))
  def createOutput(): String = results.prettyString(columns = pr.cartesian(al).map(e => e._1 + e._2).toList)
}

trait IncrementalSchemeProperties extends IncrementalProperties[SchemeExp] {
  override def interestingAddress[A <: Address](a: A): Boolean = a match {
    case PrmAddr(_) => false
    case _ => true
  }
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModFProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.scam2020ModF
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  val outputFile: String = s"properties/modf.txt"

}

object IncrementalSchemeModConcProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.scam2020ModConc
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  val outputFile: String = s"properties/modconc.txt"
}

object IncrementalSchemeModXProperties {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModFProperties.main(args)
    IncrementalSchemeModConcProperties.main(args)
  }
}
