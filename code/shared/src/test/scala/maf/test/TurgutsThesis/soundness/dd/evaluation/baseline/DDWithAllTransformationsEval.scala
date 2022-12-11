package maf.test.TurgutsThesis.soundness.dd.evaluation.baseline

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.TransformationManager
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.dd.SchemeSoundnessWithDeltaDebuggingTests
import org.scalatest.Assertions.fail

object DDWithAllTransformationsEval:
  var dataCollector = new BaselineDataCollector
  var bugName = "noneYet"
  def reduce(program: SchemeExp,
             soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
             benchmark: String,
             analysisProfiling: Array[(String, Int)]): Unit =
    var count = 0

    val startTime = System.currentTimeMillis()
    val reduced = GTR.reduce(
      program,
      p => {
        count += 1

        /**
         * without the line below, one might have undefined variables that are never needed dynamically (e.g. dead-code)
         * And that brings shallow/deep dropping into an infinite loop, since they try to drop all undefined variables
         */
        soundnessTester.runAndCompare(p, benchmark) match
          case Some((failureMsg, _, _)) =>
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty
          case None => false
      },
      identity,
      TransformationManager.allTransformations
    )

    val endTime = System.currentTimeMillis()
    dataCollector.addOriginalSize(bugName, program.size)
    dataCollector.addReducedSize(bugName, reduced.size)
    dataCollector.addReductionTime(bugName, endTime - startTime)
