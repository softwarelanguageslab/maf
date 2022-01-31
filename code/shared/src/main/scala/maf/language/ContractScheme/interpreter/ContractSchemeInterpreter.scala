package maf.language.ContractScheme.interpreter

import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.change.CodeVersion._
import maf.language.scheme.*
import maf.core.Identity
import maf.util.benchmarks.Timeout
import scala.util.control.TailCalls._
import maf.language.ContractScheme.ContractValues
import scala.concurrent._
import maf.util._

class ContractSchemeInterpreter extends SchemeInterpreter:
    import ConcreteValues.*
    import ContractSchemeErrors.*

    case class ContractValue(value: ContractValues.Value[Value]) extends Value

    override def eval(
        e: SchemeExp,
        env: Env,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
        if timeout.reached then throw new TimeoutException()
        e match
            case ContractSchemeMon(contract, expression, idn) =>
              // first evaluate the contract
              // then evaluate expression, and finally apply the contract on the expression
              for
                  evaluatedContract <- tailcall(eval(contract, env, timeout, version))
                  evaluatedExpression <- tailcall(eval(expression, env, timeout, version))
                  result <- tailcall(mon(evaluatedContract, evaluatedExpression, contract, expression, idn, timeout, version))
              yield result

            case ContractSchemeFlatContract(expression, idn) =>
              // evaluates to a flat contract
              for evaluatedExpression <- tailcall(eval(expression, env, timeout, version))
              yield ContractValue(ContractValues.Flat(evaluatedExpression, e, None, expression.idn))

            case ContractSchemeDepContract(domains, rangeMaker, idn) =>
              import TailrecUtil.*

              // evaluates to a guard
              for
                  evaluatedDomains <- sequence(domains.map(domain => tailcall(eval(domain, env, timeout, version))))
                  evaluatedRangeMaker <- tailcall(eval(rangeMaker, env, timeout, version))
              yield ContractValue(ContractValues.Grd(evaluatedDomains, evaluatedRangeMaker, domains.map(_.idn), rangeMaker))

            case ContractSchemeCheck(flat, valueExpression, idn) => ???

            case _ => super.eval(e, env, timeout, version)

    /**
     * Runs a monitor of the given contract on the given value.
     *
     * Can result in three things: the value of "value", a monitored function value (in case the contract was a guard, and the value is a procedure?),
     * or an exception if the monitor fails or one of the prerequisites of the previous cases fail.
     *
     * @param contract
     *   the contract that must be monitored in the given value
     * @param value
     *   the value that must e monitored
     * @param monIdn
     *   the identity of the mon expression. Can be Identity.none if none is available.
     */
    private def mon(
        contractv: Value,
        value: Value,
        contract: SchemeExp,
        arg: SchemeExp,
        monIdn: Identity,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] =
      // first try to see what type the contract value is
      contract match
          case ContractValue(ContractValues.Flat(flatv, _, _, _)) =>
            // it is a flat contract, so we must apply the inner function value, if return value is true the monitored value is returned, otherwise an error is thrown
            applyFun(flatv, SchemeFuncall(contract, List(arg), monIdn), List(value), List(arg), monIdn, timeout, version)
          case ContractValue(c: ContractValues.Grd[Value]) if isProcedure(value) =>
            done(ContractValue(ContractValues.Arr[Value](contract.idn, arg.idn, c, value)))
          case _ if isProcedure(contractv) =>
            // a procedure can be turned into a flat contract
            mon(
              ContractValue(ContractValues.Flat(contractv, ContractSchemeFlatContract(contract, contract.idn), None, contract.idn)),
              value,
              contract,
              arg,
              monIdn,
              timeout,
              version
            )

          // everything else is an invalid monitor
          case _ =>
            throw ContractSchemeTypeError(s"cannot monitor value $value using $contract")

end ContractSchemeInterpreter
