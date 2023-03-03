package maf.test.deltaDebugging.soundnessDD.evaluation.baseline

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.evaluation.*

object BaselineDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(program: SchemeExp,
             soundnessTester: BaselineTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var runTimes: List[(Long, Int)] = List()
    var analysisTimes: List[(Long, Int)] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        val candidateSize = p.size
        soundnessTester.runCompareAndtime(p, benchmark) match
          case (Some(failureMsg), (runTime, analysisTime)) =>
            runTimes = runTimes.::((runTime, candidateSize))
            analysisTimes = analysisTimes.::((analysisTime, candidateSize))
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            runTimes = runTimes.::((runTime, candidateSize))
            analysisTimes = analysisTimes.::((analysisTime, candidateSize))
            false
      },
      identity,
      TransformationManager.genericTransformations
    )

    val endTime = System.currentTimeMillis()
    val totalReductionTime = endTime - startTime

    val reductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origSize = program.size,
      reducedSize = reduced.size,
      reductionTime = totalReductionTime,
      reductionPercentage = 1 - (reduced.size.toDouble / program.size),
      interpreterTime = runTimes.map(_._1).sum,
      analysisTime = analysisTimes.map(_._1).sum,
      interpreterTimes = runTimes,
      analysisTimes = analysisTimes,
      interpreterPercentage = runTimes.map(_._1).sum.toDouble / totalReductionTime,
      analysisPercentage = analysisTimes.map(_._1).sum.toDouble / totalReductionTime
    )

    dataCollector.addReductionData(reductionData)
