package maf.test.deltaDebugging.soundnessDD.variants.smartReplacement

import maf.core.Identity
import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues
import maf.test.deltaDebugging.soundnessDD.variants.*

object SmartReplacementDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var analysisResults: Map[Identity, Any] = Map()

  def reduce(program: SchemeExp,
             soundnessTester: SmartReplacementTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    Replacing.anlResults = analysisResults

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runAndCompare_(p, benchmark) match
          case Some((failureMsg, anlResults)) =>
            analysisResults = anlResults
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case None =>
            false
      },
      _ => Replacing.anlResults = analysisResults,
      TransformationManager.allTransformations
    )

    Replacing.anlResults = Map()

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
