package maf.deltaDebugging.gtr.transformations.traits

import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.*
import maf.language.sexp.Value

trait Replacing:
  private var count = -1
  def newID(): String =
    count += 1
    "unique_args_" + count.toString

  def newCallerLambda(args: List[SchemeExp]) =
    val id = newID()
    SchemeLambda(
      None,
      List(Identifier(id, NoCodeIdentity)),
      List(SchemeFuncall(
        f = SchemeVar(Identifier(id, NoCodeIdentity)),
        args = args,
        idn = NoCodeIdentity,
      )),
      None,
      NoCodeIdentity)

  def callerLambdas(): List[SchemeLambda] =
    val values: List[SchemeExp] =
      List(
        SchemeValue(Value.Integer(0), NoCodeIdentity),
        SchemeValue(Value.Boolean(true), NoCodeIdentity),
      )
    val valuesToTry = values.combinations(1).toList ++ values.combinations(2).toList
    valuesToTry.map(args => newCallerLambda(args))

  def newConstantLambda(BodyExp: SchemeExp): SchemeVarArgLambda =
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(BodyExp), None, NoCodeIdentity)

  def constantLambdas(): List[SchemeVarArgLambda] = List(
    newConstantLambda(SchemeValue(Value.Integer(1), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.String("S"), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Symbol("S"), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Boolean(true), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Boolean(false), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Nil, NoCodeIdentity)),
  )

  def lambdaValues(): List[SchemeLambdaExp] =
    constantLambdas() ++ callerLambdas()

  val values: List[SchemeExp] = List(
    SchemeValue(Value.Integer(1), NoCodeIdentity),
    SchemeValue(Value.String("S"), NoCodeIdentity),
    SchemeValue(Value.Symbol("S"), NoCodeIdentity),
    SchemeValue(Value.Boolean(true), NoCodeIdentity),
    SchemeValue(Value.Boolean(false), NoCodeIdentity),
    SchemeValue(Value.Nil, NoCodeIdentity)
  )

  def allValues(): List[SchemeExp] =
    values ++ lambdaValues()

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
