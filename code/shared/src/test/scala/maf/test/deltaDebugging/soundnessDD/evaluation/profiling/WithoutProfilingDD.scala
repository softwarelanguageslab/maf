package maf.test.deltaDebugging.soundnessDD.evaluation.profiling

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.deltaDebugging.soundnessDD.evaluation.profiling.*
import maf.test.deltaDebugging.soundnessDD.evaluation.CountLambdaBindings
import org.scalatest.Assertions.fail

import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Random

object WithoutProfilingDD:
  val dataCollector = new ProfilingDataCollector

  def reduce(program: SchemeExp,
             soundnessTester: ProfilingTester,
             benchmark: String,
             initAnalysisProfiling: Array[(String, Int)]): Unit =
    var reduced = program

    var oracleTimes: List[(Long, Int)] = List()
    var analysisSteps: List[(Int, Int)] = List()
    var oracleHits = 0
    var oracleCount = 0

    val reductionStartTime = System.currentTimeMillis()

    def reduceWithProfiling(profiling: Array[(String, Int)]): Unit =
      for (i <- Random.shuffle(profiling.indices))
        GTR.reduce(
          reduced,
          p => {
            val startTime = System.currentTimeMillis() //collect
            soundnessTester.profilingRunAndCompare(p, benchmark) match
              case Some((failureMsg, runAnalysisSteps)) =>
                oracleCount += 1 //collect
                val endTime = System.currentTimeMillis() //collect
                oracleTimes = oracleTimes.::((endTime - startTime, oracleHits)) //collect
                analysisSteps = analysisSteps.::((runAnalysisSteps.map(_._2).sum, oracleHits)) //collect
                p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty
              case None => false
          },
          newReduction => {
            oracleHits += 1
            soundnessTester.profilingRunAndCompare(newReduction, benchmark) match
              case Some((_, newReductionProfiled)) =>
                reduced = newReduction
                return reduceWithProfiling(newReductionProfiled)
              case _ =>
          },
          List(ReplaceNthExpensiveFunction(profiling, i))
        )

    reduceWithProfiling(initAnalysisProfiling)

    val reductionEndTime = System.currentTimeMillis()
    val reductionTime = reductionEndTime - reductionStartTime

    val dataPoint: ReductionData = ReductionData(
      candidateFunctionCount = initAnalysisProfiling.length,
      totalFunctionCount = CountLambdaBindings.count(program),
      oracleCount = oracleCount,
      programSize = program.size,
      reducedSize = reduced.size,
      OracleHits = oracleHits,
      reductionTime = reductionTime,
      oracleTimes = oracleTimes,
      analysisSteps = analysisSteps
    )

    dataCollector.addReductionData(dataPoint)