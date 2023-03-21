package maf.test.deltaDebugging.soundnessDD.variants.killLambda

import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeLambda, SchemeValue, SchemeVar}
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.sexp.*
import maf.core.*
import maf.deltaDebugging.gtr.transformations.traits.Replacing

object DeadCodeRemover:
  type Benchmark = String

  def remove(program: SchemeExp,
             dynAnalysis: Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]],
             deadCodeTester: DeadCodeTester,
             benchmark: Benchmark
            ): SchemeExp =

    var deadLambdas: List[SchemeLambda] = List()

    program.forEach(exp => { /** Fill in dead lambdas */
      exp match
        case lambda: SchemeLambda =>
          if !dynAnalysis.keySet.exists(calledLambda => calledLambda eq lambda) then
            deadLambdas = deadLambdas.::(lambda)
        case _ =>
    })

    for (deadLambda <- deadLambdas)
      val result = removeDeadLambda(program, deadLambda, deadCodeTester, benchmark)
      if result.nonEmpty then
        return remove(result.get._1, result.get._2, deadCodeTester, benchmark)

    for (lambdaToKill <- dynAnalysis.keySet)
      val callsAndReturnValues = dynAnalysis(lambdaToKill)
      val result = killCall(program, lambdaToKill, callsAndReturnValues, deadCodeTester, benchmark)
      if result.nonEmpty then
        return remove(result.get._1, result.get._2, deadCodeTester, benchmark)

    program

  def removeDeadLambda(program: SchemeExp,
                       deadLambda: SchemeLambda,
                       deadCodeTester: DeadCodeTester,
                       benchmark: Benchmark):
    Option[(SchemeExp, Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]])] =

    val maybeRemoved = program.deleteChildren(exp => {
      exp match
        case lambda: SchemeLambda =>
          lambda eq deadLambda
        case _ => false
    })

    maybeRemoved match
      case Some(removed) =>
        val undefinedVars: List[String] = removed.findUndefinedVariables().map(id => id.name)
        val cleanedUp = removed.map(exp => {
          exp match
            case variable: SchemeVar =>
              if undefinedVars.contains(variable.id.name) then
                SchemeValue(Value.Boolean(true), NoCodeIdentity)
              else exp
            case _ => exp
        })

        val oracleResults = deadCodeTester.runAndIdentifyDeadCode(cleanedUp, benchmark)
        oracleResults match
          case (Some(tpl), _) =>
            if tpl._1.nonEmpty then
              Some((cleanedUp, tpl._2))
            else None
          case _ =>
            None
      case _ =>
        None

  def killCall(program: SchemeExp,
               lambdaToKill: SchemeLambda,
               callsAndReturnValues: Set[(SchemeFuncall, ConcreteValues.Value)],
               deadCodeTester: DeadCodeTester,
               benchmark: Benchmark):
    Option[(SchemeExp, Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]])] =
      for((call, returnValue) <- callsAndReturnValues)
        val callRemoved =
          program.map(exp => exp match
            case c: SchemeFuncall =>
              if c eql call then
                Replacing.valueToExp(returnValue) match
                  case Some(value) => value
                  case _ => SchemeValue(Value.Boolean(false), NoCodeIdentity)
              else exp
            case _ => exp)

        val oracleResults = deadCodeTester.runAndIdentifyDeadCode(callRemoved, benchmark)
        oracleResults match
          case (Some(tpl), _) =>
            if tpl._1.nonEmpty then
              return Some(callRemoved, tpl._2)
            else None
          case _ => None
      None
