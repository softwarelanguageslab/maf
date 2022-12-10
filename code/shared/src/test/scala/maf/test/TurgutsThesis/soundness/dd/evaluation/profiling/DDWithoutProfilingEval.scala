package maf.test.TurgutsThesis.soundness.dd.evaluation.profiling

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import maf.test.TurgutsThesis.soundness.dd.evaluation.{CountLambdaBindings, ProfilingDataCollector}
import org.scalatest.Assertions.fail

import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Random

object DDWithoutProfilingEval extends DeltaDebugger:
  val dataCollector = new ProfilingDataCollector

  def reduce(program: SchemeExp,
             soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
             benchmark: String,
             initAnalysisProfiling: Array[(String, Int)]): Unit =
    var reduced = program

    var oracleTimes: List[(Long, Int)] = List()
    var analysisSteps: List[(Int, Int)] = List()
    var interpreterSteps: List[(Int, Int)] = List()
    var oracleHits = 0
    var oracleCount = 0

    val reductionStartTime = System.currentTimeMillis()

    def reduceWithProfiling(profiling: Array[(String, Int)]): Unit =
      for (i <- Random.shuffle(profiling.indices))
        GTR.reduce(
          reduced,
          p => {
            val startTime = System.currentTimeMillis()
            soundnessTester.runAndCompare(p, benchmark) match
              case Some((failureMsg, runAnalysisSteps, evalSteps)) =>
                oracleCount += 1
                val endTime = System.currentTimeMillis()
                oracleTimes = oracleTimes.::((endTime - startTime, oracleHits))
                analysisSteps = analysisSteps.::((runAnalysisSteps.map(_._2).sum, oracleHits))
                interpreterSteps = interpreterSteps.::((evalSteps, oracleHits))
                p.findUndefinedVariables().isEmpty &&
                  failureMsg.nonEmpty
              case None => false
          },
          newReduction => {
            oracleHits += 1
            soundnessTester.runAndCompare(newReduction, benchmark) match
              case Some((_, newReductionProfiled, _)) =>
                reduced = newReduction
                return reduceWithProfiling(newReductionProfiled)
              case _ =>
          },
          List(ReplaceNthExpensiveFunction(profiling, i))
        )

    reduceWithProfiling(initAnalysisProfiling)

    val reductionEndTime = System.currentTimeMillis()
    val reductionTime = reductionEndTime - reductionStartTime

    dataCollector.newID()
    dataCollector.addCandidateFunctionCount(initAnalysisProfiling.length)
    dataCollector.addFunctionCount(CountLambdaBindings.count(program))
    dataCollector.addOracleHit(oracleHits)
    dataCollector.addOracleCount(oracleCount)
    dataCollector.addProgramSize(program.size)
    dataCollector.addReducedSize(reduced.size)
    dataCollector.addOracleTimes(oracleTimes)
    dataCollector.addAnalysisSteps(analysisSteps)
    dataCollector.addInterpreterSteps(interpreterSteps)
    dataCollector.addReductionTime(reductionTime)

