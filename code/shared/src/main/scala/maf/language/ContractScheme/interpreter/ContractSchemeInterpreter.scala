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
import maf.language.ContractScheme.ContractValues.StructSetterGetter
import maf.language.ContractScheme.ContractValues.StructConstructor
import maf.language.ContractScheme.ContractValues.Arr

trait RandomInputGenerator:
    /** Generate a random input, possibly under the constraint of the given set of primitive contracts */
    def generateInput(contract: Set[String] = Set()): ConcreteValues.Value

class ContractSchemeInterpreter(cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (), signalBlame: (Identity, Identity) => Unit = (_, _) => ())
    extends SchemeInterpreter(cb):
    import ConcreteValues.*
    import ContractSchemeErrors.*
    import TailrecUtil.*

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

            case ContractSchemeCheck(flat, valueExpression, idn) =>
              // a check applies the given flat contract as a function on the given value
              for
                  evaluatedFlat <- tailcall(eval(flat, env, timeout, version))
                  evaluatedValue <- tailcall(eval(valueExpression, env, timeout, version))
                  ret <- tailcall(mon(evaluatedFlat, evaluatedValue, flat, valueExpression, idn, timeout, version, isCheck = true))
              yield ret

            // Structures
            case MakeStructConstr(tag, siz, idn) =>
              done(ContractValue(ContractValues.StructConstructor(tag, siz)))

            case MakeStructPredicate(tag, idn) =>
              done(ContractValue(ContractValues.StructPredicate(tag)))

            case MakeStructGetter(tag, idx, idn) =>
              done(ContractValue(ContractValues.StructSetterGetter(tag, idx, false)))

            case MakeStructSetter(tag, idx, idn) =>
              done(ContractValue(ContractValues.StructSetterGetter(tag, idx, true)))

            case call @ SchemeFuncall(f, args, idn) =>
              for
                  fv <- tailcall(eval(f, env, timeout, version))
                  argsv <- evalArgs(args, env, timeout, version)
                  res <- applyFun(fv, call, argsv, args, idn, timeout, version)
              yield res

            // match expression
            case MatchExpr(value, clauses, _) =>
              for
                  evaluatedValue <- eval(value, env, timeout, version)
                  // create a matcher for trying the clauses
                  matcher = Matcher(evaluatedValue, (exp) => eval(exp, env, timeout, version).result)
                  // try the clauses in order, if one matches then the expression is evaluated
                  ret <- tryClauses(env, timeout, version, clauses, matcher)
              yield ret

            case _ => super.eval(e, env, timeout, version)

    private def tryClauses(env: Env, timeout: Timeout.T, version: Version, clauses: List[MatchExprClause], matcher: Matcher): TailRec[Value] =
      clauses match
          case List() => throw ContractSchemeNoMatchClauseMatched()
          case clause :: rest =>
            if matcher.matches(clause.pat) then
                val newBindings = matcher.resolveBindings
                // if it matches we need to extend the environment and sotre with the given bindings
                val newEnv = newBindings.foldLeft(env)((env, binding) =>
                    val addr = newAddr(AddrInfo.VarAddr(binding._1))
                    extendStore(addr, binding._2)
                    env + (binding._1.name -> addr)
                )
                // then evaluate the value of the clause
                evalSequence(clause.expr, newEnv, timeout, version)
            else tryClauses(env, timeout, version, rest, matcher)

    private def checkArity[T](argsv: List[T], expected: Int): Unit =
      if expected != argsv.size then throw ContractSchemeInvalidArity(argsv.size, expected)

    override def applyFun(
        f: Value,
        call: SchemeFuncall,
        argsv: List[Value],
        args: List[SchemeExp],
        idn: Identity,
        timeout: Timeout.T,
        version: Version
      ): TailRec[Value] = f match
        // Application of monitored functions
        case ContractValue(Arr(_, _, contract, f, _)) =>
          for
              // first apply the range maker on the input arguments
              // TODO: call is not entirely correct in this context, probably needs to be synthesized
              rangeContract <- tailcall(applyFun(contract.rangeMaker, call, argsv, args, idn, timeout, version))
              _ = checkArity(argsv, contract.domain.size)
              // then check whether the domain contract holds on the arguments
              checkedArguments <- sequence(argsv.zip(contract.domain).zip(args).map { case ((argv, contract), arg) =>
                tailcall(mon(contract, argv, call, arg, idn, timeout, version))
              })
              // after all checks have been performed evaluate the actual function
              ret <- tailcall(applyFun(f, call, argsv, args, idn, timeout, version))
              // then check the range contract on the return value
              // TODO: call is not entirely correct in this context, probably needs to be synthesized
              checkedRet <- tailcall(mon(rangeContract, ret, call, call, Identity.none, timeout, version))
          yield checkedRet

        // Structures
        case ContractValue(StructConstructor(tag, size)) =>
          done(ContractValue(ContractValues.Struct(tag, ArrayEq.from(argsv))))

        case ContractValue(StructSetterGetter(tag, idx, isSetter)) =>
          if isSetter then
              checkArity(argsv, 2)
              argsv(0) match
                  case ContractValue(ContractValues.Struct(tag, fields: ArrayEq[Value])) =>
                    fields.update(idx, argsv(1).asInstanceOf)
                    done(Value.Nil)

                  case v => throw ContractSchemeTypeError(s"expected a struct but got $v")
          else
              checkArity(argsv, 1)
              argsv(0) match
                  case ContractValue(ContractValues.Struct(tag, fields)) => done(fields(idx))
                  case v                                                 => throw ContractSchemeTypeError(s"expected a struct but got $v")

        case _ => super.applyFun(f, call, argsv, args, idn, timeout, version)

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
        version: Version,
        isCheck: Boolean = false,
      ): TailRec[Value] =
      // first try to see what type the contract value is
      contractv match
          case ContractValue(ContractValues.Flat(flatv, _, _, _)) =>
            // it is a flat contract, so we must apply the inner function value, if return value is true the monitored value is returned, otherwise an error is thrown
            for
                ret <- tailcall(applyFun(flatv, SchemeFuncall(contract, List(arg), monIdn), List(value), List(arg), monIdn, timeout, version))
                vlu = ret match
                    case Value.Bool(b) if !b =>
                      if isCheck then Value.Bool(false) else throw new ContractSchemeBlame(contract.idn, arg.idn, Some(monIdn))
                    case _ => if isCheck then Value.Bool(true) else value
            yield vlu

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
