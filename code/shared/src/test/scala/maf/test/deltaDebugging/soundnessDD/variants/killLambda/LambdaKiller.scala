package maf.test.deltaDebugging.soundnessDD.variants.killLambda

import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeLambda, SchemeValue, SchemeVar, SchemeVarExp}
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.sexp.*
import maf.core.*
import maf.deltaDebugging.treeDD.transformations.traits.Replacing

import scala.util.Random

object LambdaKiller:
  type Benchmark = String

  def killLambdas(program: SchemeExp,
                  dynAnalysis: Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]],
                  staticProfiling: Array[((Int, Int), Int)],
                  deadCodeTester: KillLambdaTester,
                  benchmark: Benchmark): SchemeExp =

    var lambdas = dynAnalysis.keySet.toList
    lambdas = lambdas.sortWith((l1, l2) => {
      val l1Profiling = staticProfiling.find(tpl => tpl._1 == (l1.idn.idn.line, l1.idn.idn.col))
      val l2Profiling = staticProfiling.find(tpl => tpl._1 == (l2.idn.idn.line, l2.idn.idn.col))
      l1Profiling match
        case Some(tplL1) =>
          l2Profiling match
            case Some(tplL2) =>
              tplL1._2 < tplL2._2
            case _ => true
        case _ => true
    }).reverse

    for (lambdaToKill <- dynAnalysis.keySet)
      val callsAndReturnValues = dynAnalysis(lambdaToKill)
      val result = killLambda(program, lambdaToKill, callsAndReturnValues, deadCodeTester, benchmark)
      if result.nonEmpty then
        return killLambdas(result.get._1, result.get._2, staticProfiling, deadCodeTester, benchmark)
    program

  def killLambda(program: SchemeExp,
                 lambdaToKill: SchemeLambda,
                 callsAndReturnValues: Set[(SchemeFuncall, ConcreteValues.Value)],
                 deadCodeTester: KillLambdaTester,
                 benchmark: Benchmark):
    Option[(SchemeExp, Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]])] =

      val lambdaKilled = program.deleteChildren(exp => {
        exp eq lambdaToKill
      })

      lambdaKilled match
        case Some(programVariant) =>

          val undefinedVars: List[String] = programVariant.findUndefinedVariables().map(id => id.name)

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
        case _ => None



