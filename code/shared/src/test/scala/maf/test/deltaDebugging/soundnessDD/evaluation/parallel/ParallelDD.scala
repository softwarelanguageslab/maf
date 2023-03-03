package maf.test.deltaDebugging.soundnessDD.evaluation.parallel

import maf.deltaDebugging.gtr.{GTR, GTRParallel}
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.evaluation.counting.CountingTester
import maf.test.deltaDebugging.soundnessDD.evaluation.{DataCollector, ReductionData}

object ParallelDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var maxSteps: Long = Long.MaxValue

  def reduce(program: SchemeExp,
             soundnessTester: ParallelTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var runTimes: List[(Long, Int)] = List()
    var analysisTimes: List[(Long, Int)] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTRParallel.reduce(
      program,
      p => {
        val candidateSize = p.size
        soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
          case (Some((failureMsg, evalSteps)), (runTime, analysisTime)) =>
            runTimes.synchronized {
              runTimes = runTimes.::((runTime, candidateSize)) //collect
            }
            analysisTimes.synchronized {
              analysisTimes = analysisTimes.::((analysisTime, candidateSize)) //collect
            }
            maxSteps.synchronized {
              maxSteps = Math.min(evalSteps, maxSteps)
            }
            oracleInvocations.synchronized {
              oracleInvocations += 1
            }
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            runTimes.synchronized {
              runTimes = runTimes.::((runTime, candidateSize))
            }
            analysisTimes.synchronized {
              analysisTimes = analysisTimes.::((analysisTime, candidateSize))
            }
            oracleInvocations.synchronized {
              oracleInvocations += 1
            }
            false
      },
      identity,
      TransformationManager.allTransformations
    )

    val endTime = System.currentTimeMillis()
    val totalTime = endTime - startTime

    val reductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origSize = program.size,
      reducedSize = reduced.size,
      reductionTime = totalTime,
      reductionPercentage = 1 - (reduced.size.toDouble / program.size),
      interpreterTime = runTimes.map(_._1).sum,
      analysisTime = analysisTimes.map(_._1).sum,
      interpreterTimes = runTimes,
      analysisTimes = analysisTimes,
      interpreterPercentage = runTimes.map(_._1).sum.toDouble / totalTime,
      analysisPercentage = analysisTimes.map(_._1).sum.toDouble / totalTime
    )

    dataCollector.addReductionData(reductionData)

