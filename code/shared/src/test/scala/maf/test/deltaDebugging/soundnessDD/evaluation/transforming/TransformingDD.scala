package maf.test.deltaDebugging.soundnessDD.evaluation.transforming

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.evaluation.*

object TransformingDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(program: SchemeExp,
             soundnessTester: TransformingTester,
             benchmark: String,
             origCost: (Long, Long)): Unit =

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
    val totalTime = endTime - startTime

    val reductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origSize = program.size,
      origCost = origCost,
      reducedSize = reduced.size,
      reductionTime = totalTime,
      reductionPercentage = 1 - (reduced.size.toDouble / program.size),
      interpreterTime = runTimes.sum,
      analysisTime = analysisTimes.sum,
      interpreterTimes = runTimes,
      analysisTimes = analysisTimes,
      interpreterPercentage = runTimes.sum.toDouble / totalTime,
      analysisPercentage = analysisTimes.sum.toDouble / totalTime
    )

    dataCollector.addReductionData(reductionData)

