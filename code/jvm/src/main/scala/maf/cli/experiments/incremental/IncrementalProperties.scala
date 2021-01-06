package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core._
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme._
import maf.util.datastructures.ListOps.Crossable
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalProperties[E <: Expression] extends IncrementalExperiment[E] {

  final val in: String = " (init)" // Initial analysis
  final val u1: String = " (inc1)" // Incremental update
  final val u2: String = " (inc2)" // Incremental update
  final val re: String = " (rean)" // Full reanalysis

  final val al = List(in, u1, u2, re) // Analyses

  final val co: String = "#Components" // Number of components
  final val an: String = "#Analyses" // Number of intra-component analyses
  final val ad: String = "|Store|" // Store size
  final val dp: String = "#Dependencies" // Number of dependencies

  final val pr = List(ad, co, dp, an) // Properties

  final val inf: String = "∞" // Timeout
  final val err: String = "E" // Error

  var results: Table[String] = Table.empty.withDefaultValue(" ")

  def runAnalysis(
      name: String,
      file: String,
      analysis: Analysis,
      block: Timeout.T => Unit,
      marker: String
    ): Boolean = {
    print(name)
    val timeOut = timeout()
    block(timeOut)
    if (timeOut.reached) { // We do not use the test `analysis.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.
      print("timed out.")
      pr.foreach(p => results = results.add(file, p + marker, inf))
      return false
    }
    results = results
      .add(file, co + marker, s"${analysis.visited.size}")
      //.add(file, an + marker, s"${analysis.intraCount - cnt}")
      .add(file, ad + marker, s"${analysis.store.size}")
      .add(file, dp + marker, s"${analysis.deps.values.map(_.size).sum}")
    true
  }

  def onBenchmark(file: String): Unit = {
    print(s"Testing $file ")
    val program = parse(file)

    // Initial analysis: analyse + update.
    val a1 = analysis(program)

    // Base case: analysis of new program version.
    val a2 = analysis(program)
    a2.version = New

    // Run the initial analysis.
    if (!runAnalysis("init ", file, a1, timeOut => a1.analyze(timeOut), in)) return

    val a1Copy = a1.deepCopy()

    // Update the initial analysis.
    runAnalysis("-> inc1 ", file, a1, timeOut => a1.updateAnalysis(timeOut, file, false), u1)

    // Run the second incremental update.
    runAnalysis("-> inc2 ", file, a1Copy, timeOut => a1Copy.updateAnalysis(timeOut, file, true), u2)

    // Run a full reanalysis
    runAnalysis("-> rean ", file, a2, timeOut => a2.analyze(timeOut), re)
  }

  def interestingAddress[A <: Address](a: A): Boolean

  def reportError(file: String): Unit = pr.foreach(d => al.foreach(a => results = results.add(file, d + a, err)))

  def createOutput(): String = results.prettyString(columns = pr.cartesian(al).map(e => e._1 + e._2).toList)
}

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemeProperties extends IncrementalProperties[SchemeExp] {
  override def interestingAddress[A <: Address](a: A): Boolean = a match {
    case PrmAddr(_) => false
    case _          => true
  }

  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))

  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModFProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential

  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)

  val outputFile: String = s"properties/modf-type.txt"
}

object IncrementalSchemeModFCPProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential

  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFCPAnalysisStoreOpt(e)

  val outputFile: String = s"properties/modf-CP.txt"
}

object IncrementalSchemeModConcProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads

  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)

  val outputFile: String = s"properties/modconc-type.txt"
}

object IncrementalSchemeModConcCPProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads

  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcCPAnalysisStoreOpt(e)

  val outputFile: String = s"properties/modconc-CP.txt"
}

object IncrementalSchemeModXProperties {
  def main(args: Array[String]): Unit = {
    //IncrementalSchemeModFProperties.main(args)
    IncrementalSchemeModFCPProperties.main(args)
    //IncrementalSchemeModConcProperties.main(args)
    IncrementalSchemeModConcCPProperties.main(args)
  }
}
