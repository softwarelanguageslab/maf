package maf.test.deltaDebugging.soundnessDD.variants.killLambda

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.language.scheme.{SchemeExp, SchemeLambda}
import maf.test.deltaDebugging.soundnessDD.variants.*

object KillLambdaDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(startProgram: SchemeExp,
             program: SchemeExp,
             soundnessTester: KillLambdaTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runAndFindLambdas(p, benchmark) match
          case (Some((failureMsg, calledLambdas, _)), _) =>
            //topCalledLambdas = calledLambdas
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, (runTime, analysisTime)) =>
            false
      },
      identity,
      TransformationManager.allTransformations,
    )

    val endTime = System.currentTimeMillis()
    val totalReductionTime = endTime - startTime

    val reductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origSize = startProgram.size,
      reducedSize = reduced.size,
      reductionTime = totalReductionTime,
      reductionPercentage = 1 - (reduced.size.toDouble / startProgram.size),
      oracleTreeSizes = oracleTreeSizes
    )

    dataCollector.addReductionData(reductionData)
