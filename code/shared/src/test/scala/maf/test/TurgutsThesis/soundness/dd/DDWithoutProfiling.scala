package maf.test.TurgutsThesis.soundness.dd

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.*
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import org.scalatest.Assertions.fail

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

object DDWithoutProfiling extends DeltaDebugger:
  override def reduce(program: SchemeExp,
                      soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
                      benchmark: String,
                      analysisProfiling: Array[(String, Int)]): Unit =
    ReplaceLambdaCalls.arr = analysisProfiling
    var reductionAnalysisCalls: Int = 0
    val reduced = GTR.reduce(
      program,
      p => {
        /**
         * without the line below, one might have undefined variables that are never needed dynamically (e.g. dead-code)
         * And that brings shallow/deep dropping into an infinite loop, since they try to drop all undefined variables
         */
        val (failureMsg, analysisProfiling) = soundnessTester.runAndCompare(p, benchmark)
        analysisProfiling.foreach(tpl => reductionAnalysisCalls += tpl._2)

        p.findUndefinedVariables().isEmpty &&
          failureMsg.nonEmpty
      },
      List(ReplaceLambdaCalls)
    )

    val bw = BufferedWriter(
      FileWriter(
        File("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/fileLogs/DDWithoutProfiling"),
        true
      )
    )

    bw.write(reductionAnalysisCalls.toString + "\n")
    bw.close()

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