package maf.TurgutsThesis.gtr.transformations

import maf.core.Identifier
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeVarExp}

trait CallReducing:
  def reduceCallsToId(exp: SchemeExp, id: Identifier, argIdx: Int): SchemeExp =
    exp.map(subExp => {
      subExp match
        case SchemeFuncall(f: SchemeVarExp, fArgs, idn) =>
          if f.id.name == id.name then
            SchemeFuncall(f, fArgs.take(argIdx) ++ fArgs.drop(argIdx + 1), idn)
          else subExp
        case _ => subExp
    })
