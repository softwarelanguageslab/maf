package maf.test.deltaDebugging.soundnessDD.variants.SDCE

import maf.core.*
import maf.deltaDebugging.treeDD.transformations.traits.Replacing
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.sexp.*

import scala.util.Random

object DeadCodeRemover:
  type Benchmark = String

  def removeDeadCode(program: SchemeExp,
                     dynAnalysis: Set[SchemeExp],
                     deadCodeTester: SDCE_Tester,
                     benchmark: Benchmark
            ): SchemeExp =

    val deadCodeRemoved = program.map(exp => {
      if dynAnalysis.exists(calledExp => calledExp eq exp) then
        exp
      else SchemeValue(Value.Nil, NoCodeIdentity)
    })

    val oracleResults = deadCodeTester.runAndCompare(deadCodeRemoved, benchmark)
    oracleResults match
      case Some(failureMsg) =>
        if failureMsg.nonEmpty then
          return deadCodeRemoved
      case _ =>

    program