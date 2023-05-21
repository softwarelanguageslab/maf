package maf.test.deltaDebugging.soundnessDD.variants.preHalt

import maf.deltaDebugging.treeDD.SchemeReduce
import maf.deltaDebugging.treeDD.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues
import maf.test.deltaDebugging.soundnessDD.variants.*

object PreHaltDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var problematicValue: Option[ConcreteValues.Value] = None

  def reduce(program: SchemeExp,
             soundnessTester: PreHaltTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = SchemeReduce.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runAndCompare_(p, benchmark, problematicValue) match
          case Some(failureMsg, _) =>
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case None =>
            false
      },
      identity,
      TransformationManager.allTransformations /** Uses all Transformations! */
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

