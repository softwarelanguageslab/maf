package maf.TurgutsThesis.gtr.transformations.traits

import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeValue, SchemeVar, SchemeVarArgLambda, SchemeVarExp}
import maf.language.sexp.Value

trait Replacing:
  
  private var count = -1
  def newID(): String =
    count += 1
    "unique_args_" + count.toString

  def lambdaValues(): List[SchemeVarArgLambda] = List(
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.Integer(0), NoCodeIdentity)), None, NoCodeIdentity),
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.Integer(1), NoCodeIdentity)), None, NoCodeIdentity),
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.String("S"), NoCodeIdentity)), None, NoCodeIdentity),
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.Symbol("S"), NoCodeIdentity)), None, NoCodeIdentity),
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.Boolean(true), NoCodeIdentity)), None, NoCodeIdentity),
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.Boolean(false), NoCodeIdentity)), None, NoCodeIdentity),
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(SchemeValue(Value.Nil, NoCodeIdentity)), None, NoCodeIdentity)
  )

  def values(): List[SchemeExp] = List(
    SchemeValue(Value.Integer(0), NoCodeIdentity),
    SchemeValue(Value.Integer(1), NoCodeIdentity),
    SchemeValue(Value.String("S"), NoCodeIdentity),
    SchemeValue(Value.Symbol("S"), NoCodeIdentity),
    SchemeValue(Value.Boolean(true), NoCodeIdentity),
    SchemeValue(Value.Boolean(false), NoCodeIdentity),
    SchemeValue(Value.Nil, NoCodeIdentity)
  )

  def allValues(): List[SchemeExp] = values() ++ lambdaValues()

  def replaceWithAllValues(exp: SchemeExp, toReplace: SchemeExp => Boolean): List[SchemeExp] =
    allValues().map(value => {
      replaceWithValue(exp, toReplace, value)
    })
    
  private def replaceWithValue(exp: SchemeExp, toReplace: SchemeExp => Boolean, value: SchemeExp): SchemeExp =
    exp.map(subExp => {
      if toReplace(subExp) then
        value match
          case s: SchemeVarArgLambda =>
            s.copy(vararg = Identifier(newID(), NoCodeIdentity))
          case _ => value
      else subExp
    })

  def replaceIdWithAllValues(exp: SchemeExp, id: Identifier): List[SchemeExp] =
    replaceWithAllValues(exp, subExp => {
      subExp match
        case varExp: SchemeVarExp =>
          varExp.id.name equals id.name
        case _ => false
    })

  def replaceCallWithAllValues(exp: SchemeExp, id: Identifier): List[SchemeExp] =
    replaceWithAllValues(exp, subExp => {
      subExp match
        case SchemeFuncall(f: SchemeVarExp, _, _) =>
          f.id.name equals id.name
        case varExp: SchemeVarExp =>
          varExp.id.name equals id.name
        case _ => false
    })  
