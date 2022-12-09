package maf.test.TurgutsThesis.soundness.dd

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
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
      var couldReduce = true
      while couldReduce do
        val oldReduced = reduced
        for (i <- analysisProfiling.indices)
          reduced = GTR.reduce(
            reduced,
            p => {
              soundnessTester.runAndCompare(p, benchmark) match
                case Some((failureMsg, _, _)) =>
                  p.findUndefinedVariables().isEmpty &&
                    failureMsg.nonEmpty
                case None => false
            },
            List(ReplaceNthExpensiveFunction(analysisProfiling, i))
          )
        if reduced eq oldReduced then
          couldReduce = false
        else couldReduce = true

      val parsedAgain = SchemeParser.parse(reduced.prettyString()).head //parse again, to generate file-related information (e.g. bug is at offset 20-25)
      val failureMsg = soundnessTester.runAndCompare(parsedAgain, benchmark).get._1

      fail(
        "FAILED:\n " +
          failureMsg + "\n" +
          "ORIGINAL PROGRAM: \n" +
          program.size + "\n" +
          "REDUCED PROGRAM: \n" +
          reduced.size + "\n" +
          reduced.prettyString()
      )
