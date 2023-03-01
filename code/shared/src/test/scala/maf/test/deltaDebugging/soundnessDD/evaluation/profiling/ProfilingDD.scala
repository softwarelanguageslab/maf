package maf.test.deltaDebugging.soundnessDD.evaluation.profiling

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.deltaDebugging.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.deltaDebugging.soundnessDD.evaluation.profiling.*
import maf.test.deltaDebugging.soundnessDD.evaluation.*
import org.scalatest.Assertions.fail

import java.io.{BufferedWriter, File, FileWriter}

object ProfilingDD:
  val dataCollector = new DataCollector
  var bugName = "noneYet"
  var maxSteps: Long = Long.MaxValue

  def reduce(program: SchemeExp,
             soundnessTester: ProfilingTester,
             benchmark: String,
             initAnalysisProfiling: Array[(String, Int)]): Unit =
    var reduced = program
    var oracleCount = 0

    val reductionStartTime = System.currentTimeMillis()
    
    def reduceWithProfiling(profiling: Array[(String, Int)], range: Int): Unit =
      for (i <- 0 to range)
        GTR.reduce(
          reduced,
          p => {
            val startTime = System.currentTimeMillis()
            soundnessTester.profilingRunAndCompare(p, benchmark) match
              case Some((failureMsg, _)) =>
                oracleCount += 1
                p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty
              case None => false
          },
          newReduction => {
            soundnessTester.profilingRunAndCompare(newReduction, benchmark) match
              case Some((_, newReductionProfiled)) =>
                reduced = newReduction
                return reduceWithProfiling(newReductionProfiled, range - 1)
              case _ =>
          },
          List(ReplaceNthExpensiveFunction(profiling, i))
        )

    if initAnalysisProfiling.length >= 5 then
      reduceWithProfiling(initAnalysisProfiling, 3)

    reduced = GTR.reduce(
      reduced,
      p => {
        soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
          case (Some((failureMsg, evalSteps)), _) =>
            oracleCount += 1
            maxSteps = evalSteps
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty
          case _ => false
      },
      identity,
      TransformationManager.allTransformations
    )

    val reductionEndTime = System.currentTimeMillis()
    val reductionTime = reductionEndTime - reductionStartTime

    val dataPoint: ReductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origCost = (-1, -1),
      origSize = program.size,
      reducedSize = reduced.size,
      reductionPercentage = 1 - (reduced.size.toDouble / program.size),
      reductionTime = reductionTime,
      interpreterTime = -1,
      analysisTime = -1,
      interpreterTimes = (1 to oracleCount).toList.map(n => (n, n)), //just an ad-hoc way to encode number of oracle invocations as interpreter times
      analysisTimes = (1 to oracleCount).toList.map(n => (n, n)),
      interpreterPercentage = -1,
      analysisPercentage = -1
    )

    dataCollector.addReductionData(dataPoint)