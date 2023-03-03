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
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runCompareAndtime(p, benchmark) match
          case (Some(failureMsg), (runTime, analysisTime)) =>
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
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
      oracleTreeSizes = oracleTreeSizes
    )

    dataCollector.addReductionData(reductionData)
