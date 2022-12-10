package maf.test.TurgutsThesis.soundness.dd.profiling

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import org.scalatest.Assertions.fail

import java.io.{BufferedWriter, File, FileWriter}

object DDWithProfiling extends DeltaDebugger:
  def reduce(program: SchemeExp,
             soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
             benchmark: String,
             initAnalysisProfiling: Array[(String, Int)]): Unit =
    var reduced = program

    def reduceWithProfiling(profiling: Array[(String, Int)]): Unit =
      for (i <- profiling.indices)
        GTR.reduce(
          reduced,
          p => {
            soundnessTester.runAndCompare(p, benchmark) match
              case Some((failureMsg, _, _)) =>
                p.findUndefinedVariables().isEmpty &&
                  failureMsg.nonEmpty
              case None => false
          },
          newReduction => {
            soundnessTester.runAndCompare(newReduction, benchmark) match
              case Some((_, newReductionProfiled, _)) =>
                reduced = newReduction
                return reduceWithProfiling(newReductionProfiled)
              case _ =>
          },
          List(ReplaceNthExpensiveFunction(profiling, i))
        )

    reduceWithProfiling(initAnalysisProfiling)

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
