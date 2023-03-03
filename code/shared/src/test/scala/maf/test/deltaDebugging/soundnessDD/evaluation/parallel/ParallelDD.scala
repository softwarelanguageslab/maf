package maf.test.deltaDebugging.soundnessDD.evaluation.parallel

import maf.deltaDebugging.gtr.{GTR, GTRParallel}
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.test.deltaDebugging.soundnessDD.evaluation.counting.CountingTester
import maf.test.deltaDebugging.soundnessDD.evaluation.{DataCollector, ReductionData}

object ParallelDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var maxSteps: Long = Long.MaxValue

  def reduce(program: SchemeExp,
             soundnessTester: ParallelTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTRParallel.reduce(
      program,
      p => {
        val candidateSize = p.size
        soundnessTester.runCompareAndtimeWithMaxSteps(p, benchmark, maxSteps) match
          case (Some((failureMsg, evalSteps)), _) =>
            oracleTreeSizes.synchronized {
              oracleTreeSizes = oracleTreeSizes.::(p.size)
            }
            maxSteps.synchronized {
              maxSteps = Math.min(evalSteps, maxSteps)
            }
            oracleInvocations.synchronized {
              oracleInvocations += 1
            }
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case (None, _) =>
            oracleTreeSizes.synchronized {
              oracleTreeSizes = oracleTreeSizes.::(p.size)
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

