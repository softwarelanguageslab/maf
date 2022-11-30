package maf.TurgutsThesis.gtr.transformations.traits

import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeValue, SchemeVarExp, SchemeVar, SchemeVarArgLambda}
import maf.language.sexp.Value

trait Replacing:
  
  private var count = -1
  def uniqueIdentifier(): String =
    count += 1
    "unique_args_" + count.toString
  
  def replaceWithAllValues(exp: SchemeExp, toReplace: SchemeExp => Boolean): List[SchemeExp] =
    List(
      replaceWithValue(exp, toReplace, SchemeValue(Value.Integer(1), NoCodeIdentity)),
      replaceWithValue(exp, toReplace, SchemeValue(Value.String("S"), NoCodeIdentity)),
      replaceWithValue(exp, toReplace, SchemeValue(Value.Symbol("S"), NoCodeIdentity)),
      replaceWithValue(exp, toReplace, SchemeValue(Value.Boolean(true), NoCodeIdentity)),
      replaceWithValue(exp, toReplace, SchemeValue(Value.Boolean(false), NoCodeIdentity)),
      replaceWithValue(exp, toReplace, SchemeValue(Value.Nil, NoCodeIdentity)),
      //lambdas:
      replaceWithValue(exp, toReplace,
        SchemeVarArgLambda(None, List(), Identifier(uniqueIdentifier(), NoCodeIdentity), List(SchemeValue(Value.Integer(1), NoCodeIdentity)), None, NoCodeIdentity)),
      replaceWithValue(exp, toReplace,
        SchemeVarArgLambda(None, List(), Identifier(uniqueIdentifier(), NoCodeIdentity), List(SchemeValue(Value.String("S"), NoCodeIdentity)), None, NoCodeIdentity)),
      replaceWithValue(exp, toReplace,
        SchemeVarArgLambda(None, List(), Identifier(uniqueIdentifier(), NoCodeIdentity), List(SchemeValue(Value.Symbol("S"), NoCodeIdentity)), None, NoCodeIdentity)),
      replaceWithValue(exp, toReplace,
        SchemeVarArgLambda(None, List(), Identifier(uniqueIdentifier(), NoCodeIdentity), List(SchemeValue(Value.Boolean(true), NoCodeIdentity)), None, NoCodeIdentity)),
      replaceWithValue(exp, toReplace,
        SchemeVarArgLambda(None, List(), Identifier(uniqueIdentifier(), NoCodeIdentity), List(SchemeValue(Value.Boolean(false), NoCodeIdentity)), None, NoCodeIdentity)),
      replaceWithValue(exp, toReplace,
        SchemeVarArgLambda(None, List(), Identifier(uniqueIdentifier(), NoCodeIdentity), List(SchemeValue(Value.Nil, NoCodeIdentity)), None, NoCodeIdentity)),
    )

  def replaceIdWithAllValues(exp: SchemeExp, id: Identifier): List[SchemeExp] =
    replaceWithAllValues(exp, subExp => {
      subExp match
        case varExp: SchemeVarExp =>
          varExp.id.name equals id.name
        case _ => false
    })  
    
  private def replaceWithValue(exp: SchemeExp, toReplace: SchemeExp => Boolean, value: SchemeExp): SchemeExp =
    exp.map(subExp => {
      if toReplace(subExp) then
        value
      else subExp
    })
