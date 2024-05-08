package maf.cli.experiments
package domain

import scala.util.Try
import maf.util.{Writer, Reader}
import maf.language.scheme.SchemeParser
import maf.lattice.HMapInstrument
import maf.bench.scheme.*
import maf.util.benchmarks.Table

object DomainSize:
    import SchemeBenchmarkPrograms.*
  
    type AnalysisResult = Map[Int, Int]

    val programs = (ad ++ gabriel ++ gambit ++ scp1 ++ toplas98 ++ WCR2019).toList

    private def runAnalysis(programName: String): Option[AnalysisResult] = 
      val source   = Reader.loadFile(programName)
      val program  = SchemeParser.parseProgram(source)
      val analysis = SchemeAnalyses.contextInsensitiveAnalysis(program)
      HMapInstrument.reset()
      Try(analysis.analyze()).toOption.map(_ => HMapInstrument.count)


    def main(args: Array[String]): Unit =
        val results = programs.map(runAnalysis) 
        val csv = programs.zip(results).foldLeft(Table.empty.copy(default=Some(0))) { case (table, (program, result)) => 
            result.map(_.foldLeft(table) { case (table, (size, count)) => 

              table.add(program, size.toString, count) }).getOrElse(table)}.toCSVString()

        val w = Writer.openTimeStamped("results/output.csv")
        Writer.write(w, csv)
        Writer.close(w)


