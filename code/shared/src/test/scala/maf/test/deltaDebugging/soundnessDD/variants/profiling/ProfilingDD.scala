package maf.test.deltaDebugging.soundnessDD.variants.profiling

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.deltaDebugging.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.deltaDebugging.soundnessDD.variants.profiling.*
import maf.test.deltaDebugging.soundnessDD.variants.*
import org.scalatest.Assertions.fail

import java.io.{BufferedWriter, File, FileWriter}

object ProfilingDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var maxSteps: Long = Long.MaxValue

  def reduce(program: SchemeExp,
             soundnessTester: ProfilingTester,
             benchmark: String,
             initAnalysisProfiling: Array[(String, Int)]): Unit =

    var oracleInvocations = 0
    var reduced = program
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    def reduceWithProfiling(profiling: Array[(String, Int)], range: Int): Unit =
      for (i <- 0 to range)
        GTR.reduce(
          reduced,
          p => {
            oracleTreeSizes = oracleTreeSizes.::(p.size)
            soundnessTester.profilingRunAndCompare(p, benchmark) match
              case Some((failureMsg, _)) =>
                oracleInvocations += 1
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
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
          case (Some((failureMsg, evalSteps)), (runTime, analysisTime)) =>
            maxSteps = evalSteps
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            false
      },
      identity,
      TransformationManager.allTransformations
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
      oracleTreeSizes
    )

    dataCollector.addReductionData(reductionData)
