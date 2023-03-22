package maf.test.deltaDebugging.soundnessDD.variants.DeadCodeElimination

import maf.core.*
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.scheme.*
import maf.language.sexp.*

import scala.util.Random

object DeadCodeRemover:
  type Benchmark = String

  def removeDeadLambdas(program: SchemeExp,
                        dynAnalysis: Set[SchemeExp],
                        deadCodeTester: DeadCodeTester,
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

    /*
    val deadLambdasRemoved = program.deleteChildren(exp => {
      exp match
        case l: SchemeLambda =>
          !dynAnalysis.exists(calledLambda => calledLambda eq l)
        case _ => false
    })

    deadLambdasRemoved match
      case Some(programVariant) =>
        val undefinedVars: List[String] = programVariant.findUndefinedVariables().map(id => id.name)
        val cleanedUp = programVariant.map(exp => {
          exp match
            case svar: SchemeVar =>
              if undefinedVars.contains(svar.id.name) then
                SchemeValue(Value.Boolean(true), NoCodeIdentity)
              else exp
            case _ => exp
        })

        val oracleResults = deadCodeTester.runAndCompare(cleanedUp, benchmark)
        oracleResults match
          case Some(failureMsg) =>
            if failureMsg.nonEmpty then
              return cleanedUp
          case _ =>
      case _ =>


    program
    */
