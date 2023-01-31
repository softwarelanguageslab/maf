package maf.test.TurgutsThesis.soundnessDD.evaluation.counting

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.TurgutsThesis.soundnessDD.evaluation.*

object CountingDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var maxSteps: Long = Long.MaxValue

  def reduce(program: SchemeExp,
             soundnessTester: CountingTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var runTimes: List[Long] = List()
    var analysisTimes: List[Long] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
          case (Some((failureMsg, evalSteps)), (runTime, analysisTime)) =>
            runTimes = runTimes.::(runTime) //collect
            analysisTimes = analysisTimes.::(analysisTime) //collect
            maxSteps = evalSteps
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            runTimes = runTimes.::(runTime)
            analysisTimes = analysisTimes.::(analysisTime)
            false
      },
      identity,
      TransformationManager.allTransformations
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
