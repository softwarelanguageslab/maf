package maf.modular.scv

import maf.language.scheme._
import maf.language.ContractScheme._
import maf.language.sexp.Value
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.util.benchmarks.Timeout
import maf.util.TaggedSet
import maf.core.{Identifier, Identity, Position}

/** This trait encodes the semantics of the ContractScheme language */
trait ScvBigStepSemantics extends ScvModAnalysis with ScvBaseSemantics { outer =>
  import maf.core.Monad.MonadSyntaxOps
  import maf.core.Monad.MonadIterableOps
  import maf.core.MonadStateT.{lift, unlift}
  import evalM._

  private lazy val `true?` : Prim = primitives.allPrimitives("true?")
  private lazy val `false?` : Prim = primitives.allPrimitives("false?")

  override def intraAnalysis(component: Component): IntraScvSemantics

  /**
   * Looks up the symbolic representation of the given variable, and returns it if it exists. Otherwise, returns a fresh symbolic representation for
   * the variable.
   */
  protected def lookupCache(id: Identifier): EvalM[Symbolic] =
    for
        env <- getEnv
        addr <- unit(
          env.lookup(id.name).getOrElse(throw Exception(s"variable ${id.name} not found"))
        ) // exception should not happen because of lexical address pass
        value <- lookupCache(addr).flatMap(v => v.map(unit).getOrElse(fresh))
    yield value

  extension (p: Prim)
    def symApply(args: Symbolic*): Symbolic =
      SchemeFuncall(SchemeVar(Identifier(p.name, Identity.none)), args.toList, Identity.none)

  class IntraScvSemantics(cmp: Component) extends IntraAnalysis(cmp) with IntraScvAnalysis with BaseIntraAnalysis:
      override def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val results = eval(expr(cmp)).runValue(State.empty.copy(env = fnEnv))
          writeResult(results.merge, cmp)

      /** Applies the given primitive and returns its resulting value */
      protected def applyPrimitive(prim: Prim, args: List[Value]): EvalM[Value] =
        prim.call(SchemeValue(Value.Nil, Identity.none), args)

      override def eval(exp: SchemeExp): EvalM[Value] = exp match
          // literal Scheme values have a trivial symbolic representation -> their original expression
          case SchemeValue(value, _)      => super.eval(exp).flatMap(tag(exp))
          case SchemeVar(nam)             => evalVariable(nam)
          case SchemeIf(prd, csq, alt, _) => evalIf(prd, csq, alt)
          case f @ SchemeFuncall(_, _, _) => callFun(f)

          // contract specific evaluation rules
          case ContractSchemeMon(contract, expression, idn) =>
            for
                contractVal <- extract(eval(contract))
                expressionVal <- extract(eval(expression))
                result <- applyMon(contractVal, expressionVal, expression, idn)
            yield result

          case ContractSchemeFlatContract(expression, idn) =>
            eval(expression).flatMap(value => unit(lattice.flat(ContractValues.Flat(value, expression, idn))))

          case ContractSchemeDepContract(domains, rangeMaker, idn) =>
            for
                evaluatedDomains <- domains.mapM(eval)
                evaluatedRangeMaker <- eval(rangeMaker)
            yield lattice.grd(ContractValues.Grd(evaluatedDomains, evaluatedRangeMaker, domains.map(_.idn), rangeMaker.idn))

          // catch-all, dispatching to the default Scheme semantics
          case _ => super.eval(exp)

      override def evalVariable(id: Identifier): EvalM[Value] =
        // the symbolic representation of a variable is the stored symbolic representation or a fresh symbolic variable
        lookupCache(id).flatMap(sym => super.evalVariable(id).flatMap(tag(sym)))

      /** Executes the monadic action `m` if the given condition is feasible */
      private def ifFeasible[X](prim: Prim, cnd: PostValue)(m: EvalM[X]): EvalM[X] =
        for
            primResult <- applyPrimitive(prim, List(cnd.value))
            _ <- cnd.symbolic match
                case None => unit(())
                case Some(symbolic) =>
                  extendPc(SchemeFuncall(SchemeVar(Identifier(prim.name, Identity.none)), List(symbolic), Identity.none))

            pc <- getPc
            result <-
              cnd.symbolic match
                  case _ if !lattice.isTrue(primResult) =>
                    void // if it is not possible according to the lattice, we do not execute "m"
                  case Some(symbolic) if !sat.feasible(pc) =>
                    void // if the path condition is unfeasible we also do not execute "m"
                  case Some(symbolic) =>
                    extendPc(prim.symApply(symbolic)) >>> m
                  case _ => m // if we do not have a path condition or neither of the two conditions above is fulfilled we execute "m"
        yield result

      private def symCond(prdValWithSym: PostValue, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
          val truVal = ifFeasible(`true?`, prdValWithSym)(eval(csq))
          val flsVal = ifFeasible(`false?`, prdValWithSym)(eval(alt))
          nondet(truVal, flsVal)

      protected def applyMon(
          contract: PostValue,
          expression: PostValue,
          expr: SchemeExp,
          monIdn: Identity
        ): EvalM[Value] =
          // We have three distinct possibilities for a "mon" expression:
          // 1. `contract` is a flat contract, or a function that can be treated as such, the result of mon is the value of `expression`
          // 2. `contract` is a dependent contract, in which case `expression` must be a function, the result of `mon` is a guarded function
          // 3. `contract` does not satisfy any of the above conditions, resutling in an error
          val flats = lattice.getFlats(contract.value).map(c => monFlat(c, expression, expr, monIdn))
          val guards = lattice.getGrds(contract.value).map(c => monArr(c, expression, expr, monIdn))

          nondets(flats ++ guards)

      protected def monFlat(contract: ContractValues.Flat[Value], value: PostValue, expr: SchemeExp, monIdn: Identity): EvalM[Value] =
          val call = SchemeFuncall(contract.fexp, List(expr), monIdn)
          // TODO: find better position information
          val result = applyFun(call, contract.contract, List((expr, value.value)), Position(-1, 0))
          val tru = ifFeasible(`true?`, PostValue.noSymbolic(result)) { unit(value.value) } // TODO: check whether symbolic info is available
          val fls = ifFeasible(`false?`, PostValue.noSymbolic(result)) {
            writeBlame(ContractValues.Blame(expr.idn, monIdn))
            void[Value]
          }

          nondet(tru, fls)

      protected def monArr(contract: ContractValues.Grd[Value], value: PostValue, expression: SchemeExp, monIdn: Identity): EvalM[Value] =
        // TODO: check that the value is indeed a function value, otherwise this should result in a blame (also check which blame)
        unit(lattice.arr(ContractValues.Arr(monIdn, expression.idn, contract, value.value)))

      private def applyArr(fc: SchemeFuncall, fv: PostValue): EvalM[Value] = nondets {
        lattice.getArrs(fv.value).map { arr =>
          for
              argsV <- fc.args.mapM(arg => extract(eval(arg)))
              values <- argsV.zip(arr.contract.domain).zip(fc.args).mapM { case ((arg, domain), expr) =>
                applyMon(PostValue.noSymbolic(domain), arg, expr, fc.idn)
              }
              result <- unit(applyFun(fc, arr.e, fc.args.zip(argsV.map(_.value)), fc.idn.pos))
          // TODO: also do applyMon on the result of the function call, this is only necessary because we want to extend our symbolic state based on the context range contract
          yield result
        }
      }

      private def callFun(f: SchemeFuncall): EvalM[Value] =
          val arrs = extract(super.eval(f)).flatMap(fv => applyArr(f, fv))
          val regular: EvalM[Value] = super.eval(f)
          nondet(arrs, regular)

      protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
        // the if expression is evaluated in a different way, because we use symbolic information to extend the path condition and rule out unfeasible paths
        for
            prdValWithSym <- extract(eval(prd))
            ifVal <- symCond(prdValWithSym, csq, alt)
        yield ifVal

}
