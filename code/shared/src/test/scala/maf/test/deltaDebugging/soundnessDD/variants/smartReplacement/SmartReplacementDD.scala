package maf.test.deltaDebugging.soundnessDD.variants.smartReplacement

import maf.core.Identity
import maf.deltaDebugging.treeDD.SchemeReduce
import maf.deltaDebugging.treeDD.transformations.TransformationManager
import maf.deltaDebugging.treeDD.transformations.traits.Replacing
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues
import maf.test.deltaDebugging.soundnessDD.variants.*

object SmartReplacementDD:
  var dataCollector = new DataCollector
  var bugName = "noneYet"
  var valuesMap: Map[SchemeExp, Set[ConcreteValues.Value]] = Map()
  var problematicValue: Option[ConcreteValues.Value] = None

  def reduce(program: SchemeExp,
             soundnessTester: SmartReplacementTester,
             benchmark: String): Unit =

    var oracleInvocations = 0
    var oracleTreeSizes: List[Int] = List()

    val startTime = System.currentTimeMillis()

    Replacing.valuesMap = valuesMap
    Replacing.problematicValue = problematicValue

    val reduced = SchemeReduce.reduce(
      program,
      p => {
        oracleInvocations += 1
        oracleTreeSizes = oracleTreeSizes.::(p.size)
        soundnessTester.runAndCompare_(p, benchmark) match
          case Some((failureMsg, dynAnalysis, maybeValue)) =>
            valuesMap = dynAnalysis
            problematicValue = maybeValue
            p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty
          case None =>
            false
      },
      _ => {
        Replacing.valuesMap = valuesMap
        Replacing.problematicValue = problematicValue
      },
      TransformationManager.allTransformations
    )

    Replacing.problematicValue = None
    Replacing.valuesMap = Map()

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
