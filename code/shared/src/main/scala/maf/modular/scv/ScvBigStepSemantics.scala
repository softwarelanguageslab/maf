package maf.modular.scv

import maf.language.scheme._
import maf.language.ContractScheme._
import maf.language.sexp.Value
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.util.benchmarks.Timeout
import maf.util.{MonoidInstances, TaggedSet}
import maf.core.{Identifier, Identity, Monad, Position}

/** This trait encodes the semantics of the ContractScheme language */
trait ScvBigStepSemantics extends ScvModAnalysis with ScvBaseSemantics with ScvSymbolicStore.GlobalSymbolicStore with ScvContextSensitivity { outer =>
  import maf.util.FunctionUtils.*
  import maf.core.Monad.MonadSyntaxOps
  import maf.core.Monad.MonadIterableOps
  import maf.core.MonadStateT.{lift, unlift}
  import evalM._
  import scvMonadInstance.impure

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
        value <- lookupCache(addr).flatMap(v => v.map(unit).getOrElse(fresh.flatMap(writeSymbolic(addr))))
    yield value

  extension (p: Prim)
    def symApply(args: Symbolic*): Symbolic =
      SchemeFuncall(SchemeVar(Identifier(p.name, Identity.none)), args.toList, Identity.none)

  class IntraScvSemantics(cmp: Component) extends IntraAnalysis(cmp) with IntraScvAnalysis with BaseIntraAnalysis with GlobalMapStoreIntra:
      override def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val initialState = State.empty.copy(env = fnEnv, store = initialStoreCache)
          val resultsM = for
              _ <- injectPre
              value <- extract(eval(expr(cmp)))
              _ <- checkPost(value)
          yield value

          val results = resultsM.runValue(initialState)
          writeMapAddrForce(cmp, results.vs.flatMap(_._2.symbolic).toList)
          writeResult(results.map(_.value).merge, cmp)

      /** Check the post contract on the value resulting from the analysis of the current component */
      private def checkPost(value: PostValue): EvalM[Unit] =
        usingContract(cmp) {
          case Some(_, contract, _, _) =>
            // TODO: check the monIdn parameter
            applyMon(PostValue.noSymbolic(contract), value, expr(cmp), expr(cmp).idn).flatMap(_ => unit(()))
          case None => unit(())
        }

      /** Injects the pre-condition contracts (if any are available) in the analysis of the current component */
      private def injectPre: EvalM[Unit] =
        usingContract(cmp) {
          case Some(domains, _, args, idn) =>
            for
                postArgs <- argValuesList(cmp).mapM { case (addr, arg) => fresh.flatMap(writeSymbolic(addr)).map(s => PostValue(Some(s), arg)) }

                _ <- Monad.sequence(
                  domains
                    .zip(postArgs)
                    .zip(args)
                    .map { case ((domain, arg), exp) =>
                      applyMon(PostValue.noSymbolic(domain), arg, exp, idn, assumed = true)
                    }
                )
            yield ()

          case None => unit(())
        }

      /** Computes an initial store cache based on the set of available Scheme primitives */
      protected lazy val initialStoreCache: StoreCache =
        primitives.allPrimitives.keys
          .map(name => baseEnv.lookup(name).get -> SchemeVar(Identifier(name, Identity.none)))
          .toMap

      /** Adds support for opaque values in primitives */
      override protected def applyPrimitives(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp, Value)]): Value =
          import MonoidInstances.*
          import maf.util.MonoidImplicits.*
          val values = args.map(_._2)
          lattice.join(
            super.applyPrimitives(fexp, fval, args),
            if OpqOps.eligible(values) then lattice.getPrimitives(fval).foldMap(prim => OpqOps.compute(prim, values))
            else lattice.bottom
          )

      /** Applies the given primitive and returns its resulting value */
      protected def applyPrimitive(prim: Prim, args: List[Value]): EvalM[Value] =
        prim.call(SchemeValue(Value.Nil, Identity.none), args)

      override def eval(exp: SchemeExp): EvalM[Value] = exp match
          // literal Scheme values have a trivial symbolic representation -> their original expression
          case SchemeValue(value, _)      => super.eval(exp).flatMap(tag(exp))
          case SchemeVar(nam)             => evalVariable(nam)
          case SchemeIf(prd, csq, alt, _) => evalIf(prd, csq, alt)

          // only enabled for testing, results in a nil value associated with a fresh symbol
          case SchemeFuncall(SchemeVar(Identifier("fresh", _)), List(), _) if DEBUG =>
            for
                symbolic <- fresh
                value <- tag(symbolic)(lattice.opq(ContractValues.Opq()))
            yield value

          // function calls have different behaviour in SCV as they can be guarded by contract
          case f @ SchemeFuncall(_, _, _) => callFun(f)

          // contract specific evaluation rules
          case ContractSchemeMon(contract, expression, idn) =>
            for
                contractVal <- extract(eval(contract))
                expressionVal <- extract(eval(expression))
                result <- applyMon(contractVal, expressionVal, expression, idn)
            yield result

          case ContractSchemeFlatContract(expression, idn) =>
            extract(eval(expression)).flatMap(pv => unit(lattice.flat(ContractValues.Flat(pv.value, expression, pv.symbolic, idn))))

          case ContractSchemeDepContract(domains, rangeMaker, idn) =>
            for
                evaluatedDomains <- domains.mapM(eval)
                evaluatedRangeMaker <- eval(rangeMaker)
            yield lattice.grd(ContractValues.Grd(evaluatedDomains, evaluatedRangeMaker, domains.map(_.idn), rangeMaker))

          case contractExp @ ContractSchemeCheck(_, _, _) =>
            evalCheck(contractExp)

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
            vars <- getVars
            result <-
              cnd.symbolic match
                  case _ if !lattice.isTrue(primResult) =>
                    void // if it is not possible according to the lattice, we do not execute "m"
                  case Some(symbolic) if !sat.feasible(pc, vars) =>
                    void // if the path condition is unfeasible we also do not execute "m"
                  case Some(symbolic) =>
                    extendPc(prim.symApply(symbolic)) >>> m
                  case _ => m // if we do not have a path condition or neither of the two conditions above is fulfilled we execute "m"
        yield result

      private def symCond(prdValWithSym: PostValue, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
          val truVal = ifFeasible(`true?`, prdValWithSym)(eval(csq))
          val flsVal = ifFeasible(`false?`, prdValWithSym)(eval(alt))
          nondet(truVal, flsVal)

      protected def evalCheck(checkExpr: ContractSchemeCheck): EvalM[Value] =
        for
            contract <- eval(checkExpr.contract)
            value <- eval(checkExpr.valueExpression)
            result <- nondets[Value](lattice.getFlats(contract).map { flat =>
              unit(
                applyFun(
                  SchemeFuncall(checkExpr.contract, List(checkExpr.valueExpression), Identity.none),
                  flat.contract,
                  List((checkExpr.valueExpression, value)),
                  checkExpr.idn.pos
                )
              )
            })
        yield result

      protected def applyMon(
          contract: PostValue,
          expression: PostValue,
          expr: SchemeExp,
          monIdn: Identity,
          assumed: Boolean = false
        ): EvalM[Value] =
          // We have three distinct possibilities for a "mon" expression:
          // 1. `contract` is a flat contract, or a function that can be treated as such, the result of mon is the value of `expression`
          // 2. `contract` is a dependent contract, in which case `expression` must be a function, the result of `mon` is a guarded function
          // 3. `contract` does not satisfy any of the above conditions, resutling in an error
          val flats = lattice.getFlats(contract.value).map(c => monFlat(c, expression, expr, monIdn, assumed))
          val guards = lattice.getGrds(contract.value).map(c => monArr(c, expression, expr, monIdn))

          nondets(flats ++ guards)

      protected def symCall(fn: Option[Symbolic], args: List[Option[Symbolic]]): Option[Symbolic] =
          import maf.core.OptionMonad.{given}
          for
              fnSym <- fn
              argsSym <- Monad.sequence(args)
          yield SchemeFuncall(fnSym, argsSym, Identity.none)

      protected def monFlat(
          contract: ContractValues.Flat[Value],
          value: PostValue,
          expr: SchemeExp,
          monIdn: Identity,
          assumed: Boolean = false
        ): EvalM[Value] =
          val call = SchemeFuncall(contract.fexp, List(expr), monIdn)
          // TODO: find better position information
          val result = applyFun(call, contract.contract, List((expr, value.value)), Position(-1, 0))
          val resultSymbolic = symCall(contract.sym, List(value.symbolic))
          val pv = PostValue(resultSymbolic, result)
          val tru = ifFeasible(`true?`, pv) { unit(value.value).flatMap(value.symbolic.map(tag).getOrElse(unit)) }
          val fls =
            if (!assumed) then
                ifFeasible(`false?`, pv) {
                  impure { writeBlame(ContractValues.Blame(expr.idn, monIdn)) }.flatMap(_ => void[Value])
                }
            else void

          nondet(tru, fls)

      protected def monArr(contract: ContractValues.Grd[Value], value: PostValue, expression: SchemeExp, monIdn: Identity): EvalM[Value] =
        // TODO: check that the value is indeed a function value, otherwise this should result in a blame (also check which blame)
        unit(lattice.arr(ContractValues.Arr(monIdn, expression.idn, contract, value.value)))

      private def applyArr(fc: SchemeFuncall, fv: PostValue): EvalM[Value] = nondets {
        lattice.getArrs(fv.value).map { arr =>
          for
              argsV <- fc.args.mapM(eval andThen extract)
              values <- argsV.zip(arr.contract.domain).zip(fc.args).mapM { case ((arg, domain), expr) =>
                applyMon(PostValue.noSymbolic(domain), arg, expr, fc.idn)
              }
              // apply the range maker function
              rangeContract <- unit(
                applyFun(
                  SchemeFuncall(arr.contract.rangeMakerExpr, fc.args, Identity.none),
                  arr.contract.rangeMaker,
                  fc.args.zip(argsV.map(_.value)),
                  arr.contract.rangeMakerExpr.idn.pos,
                )
              )
              result <- unit(
                applyFun(
                  fc, // syntactic function node
                  arr.e, // the function to apply
                  fc.args.zip(argsV.map(_.value)), // the arguments
                  fc.idn.pos, // the position of the function in the source code
                  Some(() => ContractCallContext(arr.contract.domain, rangeContract, fc.args, fc.idn))
                )
              )
          yield result
        }
      }

      private def callFun(f: SchemeFuncall): EvalM[Value] =
        for
            fv <- extract(eval(f.f))
            argsV <- f.args.mapM(eval andThen extract)

            result <- nondet(
              applyArr(f, fv),
              unit(applyFun(f, fv.value, f.args.zip(argsV.map(_.value)), f.idn.pos))
            ).flatMap(symCall(fv.symbolic, argsV.map(_.symbolic)).map(tag).getOrElse(unit))
        yield result

      override protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
        // the if expression is evaluated in a different way, because we use symbolic information to extend the path condition and rule out unfeasible paths
        for
            prdValWithSym <- extract(eval(prd))
            ifVal <- symCond(prdValWithSym, csq, alt)
        yield ifVal

}
