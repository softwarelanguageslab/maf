package maf.TurgutsThesis.gtr

import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeLambda, SchemeLet}

def substituteByChild(node: SchemeExp): List[SchemeExp] =
  node.subexpressions.collect {
    case s: SchemeExp => s
  }

def deleteChild(node: SchemeExp): List[SchemeExp] =
  var res: List[SchemeExp] = List()
  node match
    case SchemeLambda(name, args, body, annotation, idn) =>
      for(i <- args.indices)
        res = res.::(SchemeLambda(name, args.take(i) ++ args.drop(i + 1), body, annotation, idn))
      for(i <- body.indices)
        res = res.::(SchemeLambda(name, args, body.take(i) ++ body.drop(i + 1), annotation, idn))
      res
    case SchemeFuncall(f, args, idn) =>
      if args.nonEmpty then
        res = res.::(SchemeFuncall(args.head, args.tail, idn))
        for(i <- args.indices)
          res = res.::(SchemeFuncall(f, args.take(i) ++ args.drop(i + 1), idn))
        res
      else List()
    case SchemeLet(bindings, body, idn) =>
      for(i <- bindings.indices)
        res = res.::(SchemeLet(bindings.take(i) ++ bindings.drop(i + 1), body, idn))
      for(i <- body.indices)
        res = res.::(SchemeLet(bindings, body.take(i) ++ body.drop(i + 1), idn))
      res
    case SchemeBegin(exps, idn) =>
      for(i <- exps.indices)
        res = res.::(SchemeBegin(exps.take(i) ++ exps.drop(i + 1), idn))
      res
    case _ => List()
