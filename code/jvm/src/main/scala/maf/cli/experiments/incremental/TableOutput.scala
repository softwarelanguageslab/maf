package maf.cli.experiments.incremental

import maf.util.benchmarks.Table

trait TableOutput[R] {

  var results: Table[R]
  val error: R

  final val initS: String = "init" // Initial run.
  final val inc1S: String = "inc1" // Incremental update.
  final val inc2S: String = "inc2" // Another incremental update (same changes, different analysis).
  final val reanS: String = "rean" // Full reanalysis.

  val analysesS: List[String] = List(initS, inc1S, inc2S, reanS)
  val propertiesS: List[String]

  final val infS: String = "∞"
  final val errS: String = "E"

  def columnName(property: String, analysis: String): String = s"$property ($analysis)"

  def reportError(file: String): Unit = columns.foreach(c => results = results.add(file, c, error))

  lazy val columns: List[String] = analysesS.flatMap(a => propertiesS.map(columnName(_, a)))
}
