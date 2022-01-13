package maf.language.ContractScheme

import maf.language.scheme._
import maf.core.{Identifier, Identity}

object ContractSchemeUndefiner extends BaseSchemeUndefiner:
    import scala.util.control.TailCalls._
    import maf.util.TailrecUtil._

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

        case _: MakeStructConstr | _: MakeStructGetter | _: MakeStructSetter | _: MakeStructPredicate => done(exp)

        case ContractSchemeCheck(contract, valueExpression, idn) =>
          for
              undefineContract <- tailcall(undefine1(contract))
              undefineExpr <- tailcall(undefine1(valueExpression))
          yield valueExpression

        case _ => super.undefineExp(exp)
