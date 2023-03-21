package maf.deltaDebugging.gtr.transformations.traits

import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.sexp.Value
import maf.core.Identity
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.{HMap, HMapKey}
import maf.modular.{AnalysisResults, ModAnalysis}
import maf.modular.scheme.SchemeDomain

object Replacing:
  private var count = -1
  var valuesMap: Map[SchemeExp, Set[ConcreteValues.Value]] = Map()
  var problematicValue: Option[ConcreteValues.Value] = None

  /*
  type Analysis = ModAnalysis[SchemeExp] with AnalysisResults[SchemeExp] with SchemeDomain
  var analysis: Option[Analysis] = None
  */

  private def newID(): String =
    count += 1
    "unique_args_" + count.toString

  private def newCallerLambda(args: List[SchemeExp]) =
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

  private def callerLambdas(): List[SchemeLambda] =
    val values: List[SchemeExp] =
      List(
        SchemeValue(Value.Integer(0), NoCodeIdentity),
        SchemeValue(Value.Boolean(true), NoCodeIdentity),
      )
    val valuesToTry = values.combinations(1).toList ++ values.combinations(2).toList
    valuesToTry.map(args => newCallerLambda(args))

  private def newConstantLambda(BodyExp: SchemeExp): SchemeVarArgLambda =
    SchemeVarArgLambda(None, List(), Identifier(newID(), NoCodeIdentity), List(BodyExp), None, NoCodeIdentity)

  private def constantLambdas(): List[SchemeVarArgLambda] = List(
    newConstantLambda(SchemeValue(Value.Integer(1), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.String("S"), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Symbol("S"), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Boolean(true), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Boolean(false), NoCodeIdentity)),
    newConstantLambda(SchemeValue(Value.Nil, NoCodeIdentity)),
  )

  private def lambdaValues(): List[SchemeLambdaExp] =
    constantLambdas() ++ callerLambdas()

  private val values: List[SchemeExp] = List(
    SchemeValue(Value.Integer(1), NoCodeIdentity),
    SchemeValue(Value.String("S"), NoCodeIdentity),
    SchemeValue(Value.Symbol("S"), NoCodeIdentity),
    SchemeValue(Value.Boolean(true), NoCodeIdentity),
    SchemeValue(Value.Boolean(false), NoCodeIdentity),
    SchemeValue(Value.Nil, NoCodeIdentity)
  )

  def allSimpleValues(exp: SchemeExp): List[SchemeExp] =
    val observedValues = valuesMap.getOrElse(exp, Set()).toList
    if observedValues.isEmpty then
      values
    else
      problematicValue match
        case Some(concreteProblematicValue) =>
          val filteredProblematic = observedValues.filter(value => value equals concreteProblematicValue)
          val filteredNonProblematic = observedValues.filterNot(value => value equals concreteProblematicValue)
          val ordered = filteredProblematic ++ filteredNonProblematic //problematic ones get priority
          val expLst = ordered.flatMap(value => valueToExp(value))
          if expLst.isEmpty then
            values
          else expLst
        case None =>
          val expLst = observedValues.flatMap(value => valueToExp(value))
          if expLst.nonEmpty then
            expLst
          else values

  def allValues(exp: SchemeExp): List[SchemeExp] =
    val observedValues = valuesMap.getOrElse(exp, Set()).toList
    if observedValues.isEmpty then
      lambdaValues() ++ values
    else
      problematicValue match
        case Some(concreteProblematicValue) =>
          val filteredProblematic = observedValues.filter(value => value equals concreteProblematicValue)
          val filteredNonProblematic = observedValues.filterNot(value => value equals concreteProblematicValue)
          val ordered = filteredProblematic ++ filteredNonProblematic //problematic ones get priority
          val expLst = ordered.flatMap(value => valueToExp(value))
          if expLst.isEmpty then
            lambdaValues() ++ values
          else expLst
        case None =>
          val expLst = observedValues.flatMap(value => valueToExp(value))
          if expLst.nonEmpty then
            expLst
          else lambdaValues() ++ values

  private def replaceWithValue(exp: SchemeExp, toReplace: SchemeExp => Boolean, value: SchemeExp): SchemeExp =
    exp.map(subExp => {
      if toReplace(subExp) then
        value match
          case s: SchemeVarArgLambda =>
            s.copy(vararg = Identifier(newID(), NoCodeIdentity))
          case _ => value
      else subExp
    })

  def replaceWithAllValues(exp: SchemeExp, toReplace: SchemeExp => Boolean): List[SchemeExp] =
    allValues(exp).map(value => {
      replaceWithValue(exp, toReplace, value)
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

  def valueToExp(value: ConcreteValues.Value): Option[SchemeExp] =
    value match
      case ConcreteValues.Value.Str(str) =>
        Some(SchemeValue(Value.String(str), NoCodeIdentity))
      case ConcreteValues.Value.Pointer(_) =>
        Some(SchemeValue(Value.String("a"), NoCodeIdentity))
      case ConcreteValues.Value.Integer(n) =>
        Some(SchemeValue(Value.Integer(n), NoCodeIdentity))
      case ConcreteValues.Value.Real(r) =>
        Some(SchemeValue(Value.Real(r), NoCodeIdentity))
      case ConcreteValues.Value.Bool(b) =>
        Some(SchemeValue(Value.Boolean(b), NoCodeIdentity))
      case ConcreteValues.Value.Symbol(s) =>
        Some(SchemeValue(Value.Symbol(s), NoCodeIdentity))
      case ConcreteValues.Value.Nil =>
        Some(SchemeValue(Value.Nil, NoCodeIdentity))
      case ConcreteValues.Value.Cons(car, cdr) =>
        val maybeCar = valueToExp(car)
        val maybeCdr = valueToExp(cdr)
        (maybeCar, maybeCdr) match
          case (Some(carExp), Some(cdrExp)) =>
            Some(SchemePair(carExp, cdrExp, NoCodeIdentity))
          case _ =>
            None
      case v: ConcreteValues.Value.Clo =>
        Some(constantLambdas().head)
      case _ => None

  private def extractTypes(keySet: Set[HMapKey]): Set[String] =
    var res: Set[String] = Set()
    for (key <- keySet)
      val asString = key.toString
      if asString.contains("StrT") then
        res += "String"
      if asString.contains("IntT") then
        res += "Int"
      if asString.contains("BoolT") then
        res += "Boolean"
      if asString.contains("SymbolT") then
        res += "Symbol"
      if asString.contains("NilT") then
        res += "Nil"
    res

  private def typeToExps(t: String): List[SchemeExp] =
    t match
      case "String" =>
        List(SchemeValue(Value.String("S"), NoCodeIdentity))
      case "Int" =>
        List(SchemeValue(Value.Integer(1), NoCodeIdentity))
      case "Boolean" =>
        List(
          SchemeValue(Value.Boolean(true), NoCodeIdentity),
          SchemeValue(Value.Boolean(false), NoCodeIdentity)
        )
      case "Symbol" =>
        List(SchemeValue(Value.Symbol("S"), NoCodeIdentity))
      case "Nil" =>
        List(SchemeValue(Value.Nil, NoCodeIdentity))
      case _ => List()
