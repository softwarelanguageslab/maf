package maf.TurgutsThesis.gtr.transformations

import maf.core.{Identifier, Identity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarArgLambda, SchemeVarExp, SymbolicHole, SymbolicVar}

object DeleteChildSimple extends Transformation:
  override val name: String = "DeleteChildSimple"
  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeLambda(name, args, body, annotation, idn) =>
        for (i <- args.indices)
          addReplacement(SchemeLambda(name, args.take(i) ++ args.drop(i + 1), body, annotation, idn))
          if body.length > 1 then
            for (i <- body.indices)
              addReplacement(SchemeLambda(name, args, body.take(i) ++ body.drop(i + 1), annotation, idn))
      case SchemeFuncall(f, args, idn) =>
        if args.nonEmpty then
          addReplacement(SchemeFuncall(args.head, args.tail, idn))
          for (i <- args.indices)
            addReplacement(SchemeFuncall(f, args.take(i) ++ args.drop(i + 1), idn))
      case SchemeBegin(exps, idn) =>
        for (i <- exps.indices)
          addReplacement(SchemeBegin(exps.take(i) ++ exps.drop(i + 1), idn))
      case s: SchemeLet =>
        deleteChildLettishExp(s, SchemeLet.apply)
      case s: SchemeLetStar =>
        deleteChildLettishExp(s, SchemeLetStar.apply)
      case s: SchemeLetrec =>
        deleteChildLettishExp(s, SchemeLetrec.apply)
      case _ =>

  def deleteChildLettishExp(lettishExp: SchemeLettishExp,
                            factoryMethod: (List[(Identifier, SchemeExp)], List[SchemeExp], Identity) => SchemeLettishExp): Unit = {
    val bindings = lettishExp.bindings
    val body = lettishExp.body
    val idn = lettishExp.idn

    for (i <- body.indices)
      addReplacement(factoryMethod(bindings, body.take(i) ++ body.drop(i + 1), idn))

    for (i <- bindings.indices)
      //the line below is still needed, even when using shallow/deep dropping, because shallow/deep dropping cannot remove primitives (e.g. __top_level_cons)
      val bindingDropped = factoryMethod(bindings.take(i) ++ bindings.drop(i + 1), body, idn)
      addReplacement(bindingDropped)
  }