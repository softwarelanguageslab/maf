package maf.test.deltaDebugging.soundnessDD.variants.fitness

import maf.deltaDebugging.treeDD.SchemeReduce
import maf.deltaDebugging.treeDD.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.variants.*

import scala.util.Random

object FitnessDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(program: SchemeExp,
             soundnessTester: FitnessTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = SchemeReduce.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runCompareAndtime(p, benchmark) match
          case (Some(failureMsg), _) =>
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, _) =>
            false
      },
      identity,
      Random.shuffle(TransformationManager.allTransformations)
    )

    val endTime = System.currentTimeMillis()
    val totalTime = endTime - startTime

    val reductionData = ReductionData(
      benchmark = benchmark,
      bugName = bugName,
      origSize = program.size,
      reducedSize = reduced.size,
      reductionTime = totalTime,
      reductionPercentage = 1 - (reduced.size.toDouble / program.size),
      oracleTreeSizes = oracleTreeSizes
    )

    dataCollector.addReductionData(reductionData)

