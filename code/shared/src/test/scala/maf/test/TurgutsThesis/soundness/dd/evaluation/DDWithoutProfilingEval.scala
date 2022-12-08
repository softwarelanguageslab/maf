package maf.test.TurgutsThesis.soundness.dd.evaluation

import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.*
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import org.scalatest.Assertions.fail

import java.io.FileWriter
import java.io.File
import java.io.BufferedWriter
import scala.util.Random

object DDWithoutProfilingEval extends DeltaDebugger:
  val dataCollector = new DataCollector

  def reduce(program: SchemeExp,
             soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
             benchmark: String,
             analysisProfiling: Array[(String, Int)]): Unit =
    var reduced = program

    var oracleTimes: List[(Long, Int)] = List()
    var analysisSteps: List[(Int, Int)] = List()
    var interpreterSteps: List[(Int, Int)] = List()
    var oracleHits = 0
    var oracleCount = 0

    val reductionStartTime = System.currentTimeMillis()

    var couldReduce = true
    while couldReduce do
      val oldReduced = reduced
      for (i <- Random.shuffle(analysisProfiling.indices))
        reduced = GTR.reduce(
          reduced,
          p => {
            val startTime = System.currentTimeMillis()
            soundnessTester.runAndCompare(p, benchmark) match
              case Some((failureMsg, analysisProfiling, evalSteps)) =>
                oracleCount += 1
                val endTime = System.currentTimeMillis()

                oracleTimes = oracleTimes.::((endTime - startTime, oracleHits))
                analysisSteps = analysisSteps.::((analysisProfiling.map(_._2).sum, oracleHits))
                interpreterSteps = interpreterSteps.::((evalSteps, oracleHits))

                val bool = p.findUndefinedVariables().isEmpty &&
                  failureMsg.nonEmpty
                if bool then
                  oracleHits += 1
                bool
              case None => false
          },
          List(ReplaceNthExpensiveFunction(analysisProfiling, i))
        )
      if reduced eq oldReduced then
        couldReduce = false
      else couldReduce = true

    val reductionEndTime = System.currentTimeMillis()
    val reductionTime = reductionEndTime - reductionStartTime

    dataCollector.newID()
    dataCollector.addCandidateFunctionCount(analysisProfiling.length)
    dataCollector.addFunctionCount(CountLambdaBindings.count(program))
    dataCollector.addOracleHit(oracleHits)
    dataCollector.addOracleCount(oracleCount)
    dataCollector.addProgramSize(program.size)
    dataCollector.addReducedSize(reduced.size)
    dataCollector.addOracleTimes(oracleTimes)
    dataCollector.addAnalysisSteps(analysisSteps)
    dataCollector.addInterpreterSteps(interpreterSteps)
    dataCollector.addReductionTime(reductionTime)
