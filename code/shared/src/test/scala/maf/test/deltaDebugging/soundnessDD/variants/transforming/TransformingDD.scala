package maf.test.deltaDebugging.soundnessDD.variants.transforming

import maf.deltaDebugging.treeDD.{LayeredSchemeReduce, SchemeReduce}
import maf.deltaDebugging.treeDD.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.variants.*

object TransformingDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(program: SchemeExp,
             soundnessTester: TransformingTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = LayeredSchemeReduce.reduce(
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
      TransformationManager.allTransformations /** Uses all Transformations! */,
      None,
      7
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

