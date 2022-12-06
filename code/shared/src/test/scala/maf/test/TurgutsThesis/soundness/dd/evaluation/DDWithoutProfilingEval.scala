package maf.test.TurgutsThesis.soundness.dd.evaluation

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.ReplaceLambdaCalls
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.DeltaDebugger
import org.scalatest.Assertions.fail

object DDWithoutProfilingEval extends DeltaDebugger:
  val dataCollector = new DataCollector
  
  override def reduce(program: SchemeExp,
                      soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
                      benchmark: String,
                      analysisProfiling: Array[(String, Int)]): Unit =
    ReplaceLambdaCalls.arr = analysisProfiling
    
    var oracleTimes: List[(Long, Int)] = List()
    var analysisSteps: List[(Int, Int)] = List()
    var interpreterSteps: List[(Int, Int)] = List()
    var oracleHits = 0
    var oracleCount = 0

    val reductionStartTime = System.currentTimeMillis()
    
    val reduced = GTR.reduce(
      program,
      p => {
        oracleCount += 1
      
        val startTime = System.currentTimeMillis()
        soundnessTester.runAndCompare(p, benchmark) match
          case Some((failureMsg, analysisProfiling, evalSteps)) =>
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
      List(ReplaceLambdaCalls)
    )
    
    val reductionEndTime = System.currentTimeMillis()
    val reductionTime = reductionEndTime - reductionStartTime

    dataCollector.addOracleCount(oracleCount)
    dataCollector.addProgramSize(program.size)
    dataCollector.addReducedSize(reduced.size)
    oracleTimes.foreach(t => dataCollector.addOracleTimes(t))
    analysisSteps.foreach(s => dataCollector.addAnalysisSteps(s))
    interpreterSteps.foreach(s => dataCollector.addInterpreterSteps(s))
    dataCollector.addReductionTime(reductionTime)
