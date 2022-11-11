package maf.TurgutsThesis.gtr.transformations

import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeValue, SchemeVarExp}
import maf.language.sexp.Value

trait Replacing:
  def replaceWithAllValues(exp: SchemeExp, toReplace: SchemeExp => Boolean): List[SchemeExp] =
    List(
      replaceWithValue(exp, toReplace, Value.Integer(1)),
      replaceWithValue(exp, toReplace, Value.String("S")),
      replaceWithValue(exp, toReplace, Value.Boolean(true)),
      replaceWithValue(exp, toReplace, Value.Boolean(false)),
      replaceWithValue(exp, toReplace, Value.Symbol("S")),
    )
    
  private def replaceWithValue(exp: SchemeExp, toReplace: SchemeExp => Boolean, value: Value): SchemeExp =
    exp.map(subExp => {
      if toReplace(subExp) then
        SchemeValue(value, NoCodeIdentity)
      else subExp
    })
