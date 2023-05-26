package maf.test.deltaDebugging.soundnessDD.variants.killLambda

import maf.deltaDebugging.treeDD.SchemeReduce
import maf.deltaDebugging.treeDD.transformations.TransformationManager
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeLambda}
import maf.test.deltaDebugging.soundnessDD.variants.*

object KillLambdaDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(startProgram: SchemeExp,
             program: SchemeExp,
             soundnessTester: KillLambdaTester,
             benchmark: String,
             dynAnalysis: Map[SchemeLambda, (Set[(SchemeFuncall, Value)], Int)]): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val killResult = LambdaKiller.killLambdas(program, dynAnalysis, soundnessTester, benchmark)
    val postLambdaKills = killResult._1
    oracleTreeSizes = killResult._2

    val reduced = SchemeReduce.reduce(
      postLambdaKills,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runAndFindLambdas(p, benchmark) match
          case (Some((failureMsg, calledLambdas)), _) =>
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
