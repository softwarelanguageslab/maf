package maf.test.deltaDebugging.soundnessDD.variants.parallel

import maf.deltaDebugging.treeDD.{SchemeReduce, ParallelSchemeReduce}
import maf.deltaDebugging.treeDD.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.variants.counting.CountingTester
import maf.test.deltaDebugging.soundnessDD.variants.{DataCollector, ReductionData}

object ParallelDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"

  def reduce(program: SchemeExp,
             soundnessTester: ParallelTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = ParallelSchemeReduce.reduce(
      program,
      p => {
        val candidateSize = p.size
        soundnessTester.runCompareAndtime(p, benchmark) match
          case (Some(failureMsg), _) =>
            oracleTreeSizes.synchronized {
              oracleTreeSizes = oracleTreeSizes.::(candidateSize)
            }
            oracleInvocations.synchronized {
              oracleInvocations += 1
            }
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, _) =>
            oracleTreeSizes.synchronized {
              oracleTreeSizes = oracleTreeSizes.::(candidateSize)
            }
            oracleInvocations.synchronized {
              oracleInvocations += 1
            }
            false
      },
      identity,
      TransformationManager.allTransformations
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

