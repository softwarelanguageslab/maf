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
    var runTimes: List[Long] = List()
    var analysisTimes: List[Long] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTRParallel.reduce(
      program,
      p => {
        oracleInvocations.synchronized {
          oracleInvocations += 1
        }
        soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
          case (Some((failureMsg, evalSteps)), (runTime, analysisTime)) =>
            runTimes.synchronized {
              runTimes = runTimes.::(runTime) //collect
            }
            analysisTimes.synchronized {
              analysisTimes = analysisTimes.::(analysisTime) //collect
            }
            maxSteps.synchronized {
              maxSteps = Math.min(evalSteps, maxSteps)
            }
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            runTimes.synchronized {
              runTimes = runTimes.::(runTime)
            }
            analysisTimes.synchronized {
              analysisTimes = analysisTimes.::(analysisTime)
            }
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

