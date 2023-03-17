package maf.test.deltaDebugging.soundnessDD.variants.deadCode

import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeLambda, SchemeValue, SchemeVar}
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.sexp.*
import maf.core.*
import maf.deltaDebugging.gtr.transformations.traits.Replacing

object DeadCodeRemover:
  def remove(program: SchemeExp, dynAnalysis: Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]]): Option[SchemeExp] =

    var deadLambdas: List[SchemeLambda] = List()

    program.forEach(exp => { /** Fill in dead lambdas */
      exp match
        case lambda: SchemeLambda =>
          if !dynAnalysis.keySet.exists(calledLambda => calledLambda eq lambda) then
            deadLambdas = deadLambdas.::(lambda)
        case _ =>
    })

    if deadLambdas.nonEmpty then
      removeDeadLambdas(program, deadLambdas)
    else
      killLambda(program, dynAnalysis)

  def removeDeadLambdas(program: SchemeExp, deadLambdas: List[SchemeLambda]): Option[SchemeExp] =
    val maybeRemoved = program.deleteChildren(exp => {
      exp match
        case lambda: SchemeLambda =>
          deadLambdas.exists(dead => dead eq lambda)
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
        Some(cleanedUp)
      case _ =>
        None

  def killLambda(program: SchemeExp, dynAnalysis: Map[SchemeLambda, Set[(SchemeFuncall, ConcreteValues.Value)]]): Option[SchemeExp] =
    if dynAnalysis.keySet.isEmpty then
      None
    else
      val lambdaToKill = dynAnalysis.keySet.head
      val callsAndReturnValues = dynAnalysis(lambdaToKill)
      val maybeRemoved = program.deleteChildren(exp => {
        exp eq lambdaToKill
      })

      maybeRemoved match
        case Some(removed) =>
          val cleanedUp =
            removed.map(exp => exp match
              case call: SchemeFuncall =>
                val called = callsAndReturnValues.find(tpl => tpl._1 eq call)
                if called.nonEmpty then
                  Replacing.valueToExp(called.get._2) match
                    case Some(value) => value
                    case _ => exp
                else exp
              case _ => exp
            )
          Some(cleanedUp)
        case _ =>
          None







