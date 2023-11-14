package maf.test.deltaDebugging.soundnessDD.implementation

import maf.deltaDebugging.treeDD.*
import maf.deltaDebugging.treeDD.transformations.*
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import org.scalatest.Assertions.fail

object DD:
  var maxSteps: Long = Long.MaxValue
  def reduce(program: SchemeExp,
             soundnessTester: DDTester,
             benchmark: String): Unit =
      val reduced: SchemeExp = SchemeReduce.reduce(
        program,
        p => {
          soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
            case (Some((failureMsg, evalSteps)), (runTime, analysisTime)) =>
              maxSteps.synchronized {
                maxSteps = Math.min(evalSteps, maxSteps)
              }
              p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

            case (None, (runTime, analysisTime)) =>
              false
        },
        identity,
        TransformationManager.allTransformations
      )

      val parsedAgain = SchemeParser.parse(reduced.prettyString()).head //parse again, to generate file-related information (e.g. bug is at offset 20-25)
      val failureMsg = soundnessTester.runAndCompare(parsedAgain, benchmark).get

      fail(
        "FAILED:\n " +
          failureMsg + "\n" +
          "ORIGINAL PROGRAM: \n" +
          program.size + "\n" +
          "REDUCED PROGRAM: \n" +
          reduced.size + "\n" +
          reduced.prettyString()
      )