package maf.test.deltaDebugging.soundnessDD.variants.SDCE

import maf.deltaDebugging.gtr.GTR
import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues
import maf.test.deltaDebugging.soundnessDD.variants.*

object SDCE_DD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var problematicValue: Option[ConcreteValues.Value] = None
  var dynAnalysis: Set[SchemeExp] = Set()

  def reduce(program: SchemeExp,
             soundnessTester: SDCE_Tester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    val reduced = GTR.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runAndCompare_(p, benchmark, problematicValue) match
          case Some(failureMsg, _, set) =>
            dynAnalysis = set
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty

          case None =>
            false
      },
      identity,
      TransformationManager.allTransformations, /** Uses all Transformations! */
      Some(newCandidate => Some(DeadCodeRemover.removeDeadLambdas(newCandidate, dynAnalysis, soundnessTester, benchmark)))
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
