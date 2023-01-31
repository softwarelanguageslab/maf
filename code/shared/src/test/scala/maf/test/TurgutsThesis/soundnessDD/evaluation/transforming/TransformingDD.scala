package maf.test.TurgutsThesis.soundnessDD.evaluation.transforming

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.TurgutsThesis.soundnessDD.evaluation.*

object TransformingDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(program: SchemeExp,
             soundnessTester: TransformingTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var runTimes: List[Long] = List()
    var analysisTimes: List[Long] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        soundnessTester.runCompareAndtime(p, benchmark) match
          case (Some(failureMsg), (runTime, analysisTime)) =>
            runTimes = runTimes.::(runTime)
            analysisTimes = analysisTimes.::(analysisTime)
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            runTimes = runTimes.::(runTime)
            analysisTimes = analysisTimes.::(analysisTime)
            false
      },
      identity,
      TransformationManager.allTransformations /** Uses all Transformations! */
    )

    val endTime = System.currentTimeMillis()

    val reductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origSize = program.size,
      reducedSize = reduced.size,
      reductionTime = endTime - startTime,
      interpreterTime = runTimes.sum,
      analysisTime = analysisTimes.sum,
      interpreterTimes = runTimes,
      analysisTimes = analysisTimes
    )

    dataCollector.addReductionData(reductionData)

