package maf.test.TurgutsThesis.soundness.dd

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.*
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import org.scalatest.Assertions.fail

import java.io.FileWriter
import java.io.File
import java.io.BufferedWriter

object DDWithProfiling extends DeltaDebugger:
  def reduce(program: SchemeExp,
             soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
             benchmark: String,
             analysisProfiling: Array[(String, Int)]): Unit =
      var reduced = program
      var reductionAnalysisCalls: Int = 0
      var couldReduce = true
      while couldReduce do
        val oldReduced = reduced
        for (i <- analysisProfiling.indices)
          reduced = GTR.reduce(
            reduced,
            p => {
              val (failureMsg, analysisProfiling) = soundnessTester.runAndCompare(p, benchmark)
              analysisProfiling.foreach(tpl => reductionAnalysisCalls += tpl._2)

              p.findUndefinedVariables().isEmpty &&
              failureMsg.nonEmpty
            },
            List(ReplaceNthExpensiveFunction(analysisProfiling, i))
          )
        if reduced eq oldReduced then
          couldReduce = false
        else couldReduce = true

      println("analysis worklist puts during reduction: " + reductionAnalysisCalls)

    /*
      val bw = BufferedWriter(
        FileWriter(
          File("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/fileLogs/DDWithProfiling"),
          true
        )
      )

      bw.write(reductionAnalysisCalls.toString + "\n")
      bw.close()
      */

      val parsedAgain = SchemeParser.parse(reduced.prettyString()).head //parse again, to generate file-related information (e.g. bug is at offset 20-25)
      val failureMsg = soundnessTester.runAndCompare(parsedAgain, benchmark)._1

      fail(
        "FAILED:\n " +
          failureMsg + "\n" +
          "ORIGINAL PROGRAM: \n" +
          program.size + "\n" +
          "REDUCED PROGRAM: \n" +
          reduced.size + "\n" +
          reduced.prettyString()
      )
