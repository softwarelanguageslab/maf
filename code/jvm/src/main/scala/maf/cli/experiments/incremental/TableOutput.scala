package maf.cli.experiments.incremental

import maf.modular.incremental.IncrementalConfiguration
import maf.util.benchmarks.Table

trait TableOutput[R]:

    var results: Table[R]
    val error: R

    val configurations: List[IncrementalConfiguration]

    final val initS: String = "init" // Initial run.
    final val reanS: String = "rean" // Full reanalysis.

    lazy val analysesS: List[String] = List(initS, reanS) ++ configurations.map(_.toString)
    val propertiesS: List[String]

    final val infS: String = "∞"
    final val errS: String = "E"

    def columnName(property: String, analysis: String): String = s"$property ($analysis)"

    def reportError(file: String): Unit = columns.foreach(c => results = results.add(file, c, error))

    lazy val columns: List[String] = analysesS.flatMap(a => propertiesS.map(columnName(_, a)))
