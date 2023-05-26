package maf.test.deltaDebugging.soundnessDD.variants.killLambda

import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeLambda, SchemeValue, SchemeVar, SchemeVarExp}
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.sexp.*
import maf.core.*
import maf.deltaDebugging.treeDD.transformations.traits.Replacing

import scala.util.Random

object LambdaKiller:
  type Benchmark = String
  private var oracleTreeSizes: List[Int] = List()

  def killLambdas(program: SchemeExp,
                  dynAnalysis: Map[SchemeLambda, (Set[(SchemeFuncall, ConcreteValues.Value)], Int)],
                  deadCodeTester: KillLambdaTester,
                  benchmark: Benchmark): (SchemeExp, List[Int]) =
    oracleTreeSizes = List()

    var lambdas = dynAnalysis.toList
    lambdas = lambdas.sortBy(l => {
      l._2._2
    })

    for (lambdaToKill <- dynAnalysis.keySet)
      //val callsAndReturnValues = dynAnalysis(lambdaToKill)
      val result = killLambda(program, lambdaToKill, deadCodeTester, benchmark)
      if result.nonEmpty then
        return killLambdas(result.get._1, result.get._2, deadCodeTester, benchmark)
    (program, oracleTreeSizes)

  def killLambda(program: SchemeExp,
                 lambdaToKill: SchemeLambda,
                 deadCodeTester: KillLambdaTester,
                 benchmark: Benchmark):
    Option[(SchemeExp, Map[SchemeLambda, (Set[(SchemeFuncall, ConcreteValues.Value)], Int)])] =

      val lambdaKilled = program.deleteChildren(exp => {
        exp eq lambdaToKill
      })

      lambdaKilled match
        case Some(programVariant) =>
          val undefinedVars: List[String] = programVariant.findUndefinedVariables().map(id => id.name)
          val lambdaName = lambdaToKill.name
          lambdaName match
            case Some(name) =>
              val callsReplaced = Replacing.replaceCallWithAllValues(programVariant, Identifier(name, NoCodeIdentity))
              for (candidate <- callsReplaced) {
                val oracleResults = deadCodeTester.runAndFindLambdas(candidate, benchmark)
                oracleTreeSizes = oracleTreeSizes.::(candidate.size)
                oracleResults match
                  case (Some(tpl), _) =>
                    if tpl._1.nonEmpty then
                      return Some(candidate, tpl._2)
                  case _ =>
              }
              None
            case _ => None

          /*
          val callsReplaced = programVariant.map(exp => {
            exp match
              case call: SchemeFuncall =>
                val returnValues: Set[ConcreteValues.Value] = callsAndReturnValues.filter(tpl => tpl._1 eql call).map(tpl => tpl._2)
                var converted = returnValues.flatMap(value => Replacing.valueToExp(value))
                converted = Random.shuffle(converted)
                if converted.isEmpty then
                  exp
                else
                  converted.head
              case schemeVarExp: SchemeVarExp =>
                if undefinedVars.contains(schemeVarExp.id.name) then
                  SchemeValue(Value.Boolean(false), NoCodeIdentity)
                else exp
              case _ => exp
          })

          val oracleResults = deadCodeTester.runAndFindLambdas(callsReplaced, benchmark)
          oracleResults match
            case (Some(tpl), _) =>
              if tpl._1.nonEmpty then
                return Some(callsReplaced, tpl._2)
            case _ =>
          None
          */
        case _ => None



