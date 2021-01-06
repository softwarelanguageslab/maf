package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core._
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.modular.scheme._
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalPrecision[E <: Expression] extends IncrementalExperiment[E] {

  final val eq: String = "Equal"        // Precision of incremental update equals the one of a full reanalysis.
  final val mp: String = "More precise" // Precision of incremental update is better than the one of a full reanalysis.
  final val lp: String = "Less precise" // Precision of incremental update is lower than the one of a full reanalysis.

  final val options: List[String] = List(eq, lp, mp)

  final val i1: String = "inc1"         // First incremental analysis.
  final val i2: String = "inc2"         // Second incremental analysis.

  final val analyses: List[String] = List(i1, i2)

  val columns: List[String] = analyses.flatMap(a => options.map(o => s"$o ($a)"))
  
  final val inf: String = "∞"
  final val err: String = "E"

  var results: Table[String] = Table.empty.withDefaultValue(" ")

  def runAnalysis(name: String, block: Timeout.T => Unit): Boolean = {
    print(name)
    val timeOut = timeout()
    block(timeOut)
    timeOut.reached // We do not use the test `analysis.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.
  }

  def compareAnalyses(name: String, file: String, inc: Analysis, rean: Analysis): Unit = {
    // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
    val iStore = inc.store.withDefaultValue(inc.lattice.bottom)
    val rStore = rean.store.withDefaultValue(rean.lattice.bottom)

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
      else if (inc.lattice.subsumes(incr, rean.asInstanceOf[inc.Value]))
        l += 1 // The incremental value subsumes the value of the full reanalysis => less precise.
      else {
        System.err.println(s"$a: $incr < $rean") // Soundness error.
        System.err.flush()
        m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
      }
    })
    results = results
      .add(file, s"$eq ($name)", Formatter.withPercent(e, t))
      .add(file, s"$lp ($name)", Formatter.withPercent(l, t))
      .add(file, s"$mp ($name)", Formatter.withPercent(m, t))
  }

  def onBenchmark(file: String): Unit = {
    print(s"Testing $file ")
    val program = parse(file)
    // Initial analysis: analyse + update.
    val a1 = analysis(program)

    // Base case: analysis of new program version.
    val a2 = analysis(program)
    a2.version = New

    // Run the initial analysis and full reanalysis. They both need to finish.
    if (runAnalysis("init ", {timeOut => a1.analyze(timeOut)}) || runAnalysis("-> rean ", {timeOut => a2.analyze(timeOut)})) {
      print("timed out.")
      columns.foreach(c => results = results.add(file, c, inf))
      return
    }

    val a1Copy = a1.deepCopy()

    // First incremental update.
   /* if (!runAnalysis("-> inc1 ", {timeOut => a1.updateAnalysis(timeOut, file, false)})) compareAnalyses(i1, file, a1, a2)
    else {
      options.foreach(o => results = results.add(file, s"$o ($i1)", inf))
      print("timed out")
    } */

    // Second incremental update.
    if (!runAnalysis("-> inc2 ", {timeOut => a1Copy.updateAnalysis(timeOut, file,true)})) compareAnalyses(i2, file, a1Copy, a2)
    else {
      options.foreach(o => results = results.add(file, s"$o ($i2)", inf))
      print("timed out")
    }
  }

  // Note, we could also compare to the initial analysis. This would give us an idea on how many addresses were refined (column "More precise").

  def interestingAddress[A <: Address](a: A): Boolean
  def reportError(file: String): Unit = columns.foreach(c => results = results.add(file, c, err))
  def createOutput(): String = results.prettyString(columns = columns)
}


/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */


trait IncrementalSchemePrecision extends IncrementalPrecision[SchemeExp] {
  override def interestingAddress[A <: Address](a: A): Boolean = a match {
    case PrmAddr(_) => false
    case _ => true
  }
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModFPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  val outputFile: String = "precision/modf-type.txt"
}

object IncrementalSchemeModFCPPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFCPAnalysis(e)
  val outputFile: String = "precision/modf-CP.txt"
}

object IncrementalSchemeModFCPPrecisionStoreOpt extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFCPAnalysisStoreOpt(e)
  val outputFile: String = "precision/modf-CP-StoreOpt.txt"
}

object IncrementalSchemeModConcPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  val outputFile: String = "precision/modconc-type.txt"
}

object IncrementalSchemeModConcCPPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcCPAnalysis(e)
  val outputFile: String = "precision/modconc-CP.txt"
}

object IncrementalSchemeModConcCPPrecisionStoreOpt extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcCPAnalysisStoreOpt(e)
  val outputFile: String = "precision/modconc-CP-StoreOpt.txt"
}

object IncrementalSchemeModXPrecision {
  def main(args: Array[String]): Unit = {
    //IncrementalSchemeModFPrecision.main(args)
    //IncrementalSchemeModFCPPrecision.main(args)
    IncrementalSchemeModFCPPrecisionStoreOpt.main(args)
    //IncrementalSchemeModConcPrecision.main(args)
    //IncrementalSchemeModConcCPPrecision.main(args)
    IncrementalSchemeModConcCPPrecisionStoreOpt.main(args)
  }
}
