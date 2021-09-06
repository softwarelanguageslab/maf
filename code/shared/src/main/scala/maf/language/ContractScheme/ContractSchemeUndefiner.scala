package maf.language.ContractScheme

import maf.language.scheme._
import maf.core.{Identifier, Identity}

object ContractSchemeUndefiner extends BaseSchemeUndefiner:
  import scala.util.control.TailCalls._
  import maf.util.TailrecUtil._

  override def undefine(exps: List[SchemeExp], defs: List[(Identifier, SchemeExp)], idn: Option[Identity]): TailRec[SchemeExp] = exps match
    case ContractSchemeDefineContract(
          name,
          params,
          contract,
          expression,
          _
        ) :: rest =>
      (for
        bodyv <- tailcall(undefineBody(List(expression)))
        contractv <- tailcall(undefine1(contract))
      yield SchemeDefineVariable(
        name,
        ContractSchemeMon(contractv, SchemeLambda(Some(name.name), params, bodyv, expression.idn), exps.head.idn),
        exps.head.idn
      )).flatMap(expr => undefine(expr :: rest, defs, idn))

    case _ => super.undefine(exps, defs, idn)

  override def undefineExp(exp: SchemeExp): TailRec[SchemeExp] = exp match
    case ContractSchemeDepContract(domains, rangeMaker, idn) =>
      for
        undefinedDomains <- sequence(domains.map(d => tailcall(undefine1(d))))
        undefinedRangeMaker <- tailcall(undefine1(rangeMaker))
      yield ContractSchemeDepContract(undefinedDomains, undefinedRangeMaker, idn)

    case ContractSchemeFlatContract(expr, idn) => tailcall(undefine1(expr)).map(ContractSchemeFlatContract(_, idn))
    case ContractSchemeMon(contract, expr, idn) =>
      for
        undefineContract <- tailcall(undefine1(contract))
        undefineExpr <- tailcall(undefine1(expr))
      yield ContractSchemeMon(undefineContract, undefineExpr, idn)

    case _ => super.undefineExp(exp)
