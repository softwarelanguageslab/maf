package maf.TurgutsThesis.gtr.transformations

import maf.core.{Identifier, Identity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarArgLambda, SchemeVarExp, SymbolicHole, SymbolicVar}

def deleteChildLettishExp(lettishExp: SchemeLettishExp,
                          factoryMethod: (List[(Identifier, SchemeExp)], List[SchemeExp], Identity) => SchemeLettishExp): List[SchemeExp] = {
  var res: List[SchemeExp] = List()
  val bindings = lettishExp.bindings
  val body = lettishExp.body
  val idn = lettishExp.idn

  for (i <- body.indices)
    res = res.::(factoryMethod(bindings, body.take(i) ++ body.drop(i + 1), idn))

  for (i <- bindings.indices)
    def deleteFnc(exp: SchemeExp): Boolean =
      exp match
        case exp: SchemeLambdaExp =>
          exp.body.forall(deleteFnc)
        case SchemeFuncall(f, args, idn) =>
          deleteFnc(f) && args.forall(deleteFnc)
        case SchemeIf(cond, cons, alt, idn) =>
          deleteFnc(cond) &&
            deleteFnc(cons) &&
            deleteFnc(alt)
        case exp: SchemeLettishExp =>
          exp.body.forall(deleteFnc)
        case exp: SchemeSetExp =>
          deleteFnc(exp.value)
        case SchemeBegin(exps, idn) =>
          exps.forall(deleteFnc)
        case SchemeDefineVariable(name, value, idn) =>
          deleteFnc(value)
        case exp: SchemeVarExp =>
          exp.id eql bindings(i)._1
        case SchemeAssert(exp, idn) =>
          deleteFnc(exp)
        case _ => false

    val bindingAndReferencesDropped = factoryMethod(bindings.take(i) ++ bindings.drop(i + 1), body, idn).deleteChildren(deleteFnc)
    res = res.::(bindingAndReferencesDropped)

  res
}

def deleteChild(node: SchemeExp): List[SchemeExp] =
  var res: List[SchemeExp] = List()
  node match
    case SchemeLambda(name, args, body, annotation, idn) =>
      for (i <- args.indices)
        res = res.::(SchemeLambda(name, args.take(i) ++ args.drop(i + 1), body, annotation, idn))
        if body.length > 1 then
          for (i <- body.indices)
            res = res.::(SchemeLambda(name, args, body.take(i) ++ body.drop(i + 1), annotation, idn))
      res
    case SchemeFuncall(f, args, idn) =>
      if args.nonEmpty then
        res = res.::(SchemeFuncall(args.head, args.tail, idn))
        for (i <- args.indices)
          res = res.::(SchemeFuncall(f, args.take(i) ++ args.drop(i + 1), idn))
        res
      else List()
    case SchemeBegin(exps, idn) =>
      for (i <- exps.indices)
        res = res.::(SchemeBegin(exps.take(i) ++ exps.drop(i + 1), idn))
      res
    case s: SchemeLet =>
      deleteChildLettishExp(s, SchemeLet.apply)
    case s: SchemeLetStar =>
      deleteChildLettishExp(s, SchemeLetStar.apply)
    case s: SchemeLetrec =>
      deleteChildLettishExp(s, SchemeLetrec.apply)
    case _ => List()
