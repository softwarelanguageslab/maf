package maf.modular.contracts
import maf.core.Identity
import maf.language.contracts.ScLattice._
import maf.language.contracts.{ScExp, _}
import maf.language.sexp.{ValueBoolean, ValueInteger}
import maf.util.benchmarks.Timeout
import maf.language.sexp.ValueSymbol
import maf.language.sexp.ValueNil
import maf.language.sexp.ValueReal
import maf.language.sexp.ValueCharacter
import maf.language.sexp.ValueString

trait ScBigStepSemanticsScheme extends ScModSemanticsScheme with ScSchemePrimitives with ScSchemeSemanticsMonad {

  private val primTrue = primMap("true?")
  private val primFalse = primMap("false?")
  private val primProc = primMap("procedure?")
  private val primDep = primMap("dependent-contract?")
  private var totalRuns = 0

  import ScEvalM._

  trait IntraScBigStepSemantics extends IntraScAnalysisScheme {

    def withStoreCacheAdapter[A](f: StoreCacheAdapter => (A, StoreCacheAdapter)): ScEvalM[A] = ScEvalM.ScEvalM { context =>
      val (result, updatedStore) = f(StoreCacheAdapter(context.cache, StoreAdapter))
      Set((context.copy(cache = updatedStore.cache), result))
    }

    /**
     * Compute the context of the current component
     * @return a new context based on the environment of the component under analysis
     */
    def initialContext: Context = {
      var storeCache: StoreCache = componentStore.view.mapValues(v => (v, ScNil())).toMap

      primBindings.foreach { case (name, addr) =>
        val value = readPure(addr, storeCache)
        storeCache = storeCache + (addr -> ((value, ScIdentifier(name, Identity.none))))
        storeCache += (ScPrimAddr(name) -> ((lattice.schemeLattice.primitive(primMap(name)), ScIdentifier(name, Identity.none))))
      }

      fnEnv.mapAddrs { (addr) =>
        val value = readPure(addr, storeCache)
        if (lattice.isDefinitelyOpq(value)) {
          storeCache += (addr -> ((value, ScIdentifier(ScModSemantics.genSym, Identity.none))))
        }
        addr
      }

      Context(env = fnEnv, cache = storeCache, pc = ScNil())
    }

    def analyze(_ignored_timeout: Timeout.T): Unit = {
      totalRuns += 1
      if (totalRuns > 100) {
        throw new Exception("Timeout exceeded")
      }

      println("================================")
      println(s"Analyzing component $component")

      val (value, sstore, symbolicReturnValues) = compute(eval(fnBody))(initialContext)
      writeReturnStore(sstore)
      writeResult(value, component)

      println(s"Return value $value")
      println("==================================")
    }

    def eval(expr: ScExp): ScEvalM[PostValue] = expr match {
      case ScBegin(expressions, _)                              => evalSequence(expressions)
      case ScIf(condition, consequent, alternative, _)          => evalIf(condition, consequent, alternative)
      case ScLetRec(idents, bindings, body, _)                  => evalLetRec(idents, bindings, body)
      case ScRaise(_, _)                                        => ???
      case ScSet(variable, value, _)                            => evalSet(variable, value)
      case ScFunctionAp(ScIdentifier("and", _), operands, _, _) => evalAnd(operands)
      case ScFunctionAp(operator, operands, _, _)               => evalFunctionAp(operator, operands)
      case v: ScValue                                           => evalValue(v)
      case exp: ScIdentifier                                    => evalIdentifier(exp)
      case ScMon(contract, expression, idn, _)                  => evalMon(contract, expression, idn)
      case ScOpaque(_, refinements)                             => evalOpaque(refinements)
      case ScHigherOrderContract(domain, range, idn)            => eval(higherOrderToDependentContract(domain, range, idn))
      case ScDependentContract(domains, rangeMaker, _)          => evalDependentContract(domains, rangeMaker)
      case ScFlatContract(expression, _)                        => evalFlatContract(expression)
      case ScLambda(params, body, idn)                          => evalLambda(params, body, idn)
      case ScAssume(identifier, assumption, expression, _)      => evalAssume(identifier, assumption, expression)
      case ScProgram(expressions, _)                            => evalProgram(expressions)
      case ScDefine(variable, expression, _)                    => evalDefine(variable, expression)
      case ScDefineFn(name, parameters, body, idn)              => evalDefineFn(name, parameters, body, idn)
      case ScDefineAnnotatedFn(name, parameters, contract, expression, idn) =>
        evalDefineAnnotatedFn(name, parameters, contract, expression, idn)
      case ScProvideContracts(variables, contracts, _) => evalProvideContracts(variables, contracts)
      case ScCons(car, cdr, _)                         => evalCons(car, cdr)
      case ScCar(pai, _)                               => evalCar(pai)
      case ScCdr(pai, _)                               => evalCdr(pai)
      case ScNil(_)                                    => result(lattice.schemeLattice.nil)
    }

    /**
     * Read a value from the store cache, if it is not in the store cache, retrieve it from the global store
     * @param addr the addres to read from
     * @return a value combined with (optionally) its symbolic component
     */
    def read(addr: Addr): ScEvalM[PostValue] = withStoreCache { (cache) =>
      cache.get(addr) match {
        case Some(value)                  => pure(value)
        case None if GLOBAL_STORE_ENABLED => result(readAddr(addr))
        // if we are not using a global store, then we throw an exception
        // this should never occur, but if it does, it is much easier to track down.
        case None =>
          throw new Exception(s"Addr $addr is not found in store $cache while analysing $component")
      }
    }

    def cacheContains(addr: Addr): ScEvalM[Boolean] = withStoreCache { cache =>
      pure(cache.get(addr).isDefined)
    }

    /**
     * Write a value to both the store cache and the global store
     * @param addr the address to write the value to
     * @param value the value to write
     */
    def write(addr: Addr, value: PostValue): ScEvalM[()] =
      for {
        _ <- effectful(if (GLOBAL_STORE_ENABLED) writeAddr(addr, value._1))
        _ <- writeLocal(addr, value)
      } yield ()

    def writeForce(addr: Addr, value: PostValue): ScEvalM[()] =
      for {
        _ <- effectful(if (GLOBAL_STORE_ENABLED) forceWrite(addr, value._1))
        _ <- writeLocalForce(addr, value)
      } yield ()

    def writeLocal(addr: Addr, value: PostValue): ScEvalM[()] =
      // this prevents the local cache to be out of sync with the global store
      cacheContains(addr).flatMap { (contains) =>
        if (contains) {
          // if we already had something in the local cache on the given adress we simply
          // join that with the new value we got
          joinInCache(addr, value)
        } else if (GLOBAL_STORE_ENABLED) {
          // if the cache did not contain a value yet, then it might be in the global
          // store (that is, if we are using one)
          joinInCache(addr, (readAddr(addr), ScNil()))
            .flatMap(_ => joinInCache(addr, value))
        } else {
          throw new Exception(s"value should have been in  the cache but was not at addr $addr")
        }
      }

    def writeLocalForce(addr: Addr, value: PostValue): ScEvalM[()] =
      addToCache(addr -> value)

    def blame[X](blamedIdentity: Identity, blamingIdentity: Identity = Identity.none): ScEvalM[X] =
      withIgnoredIdentities { ignored =>
        if (!ignored.contains(blamedIdentity)) {
          writeBlame(Blame(blamedIdentity, blamingIdentity))
        }
      } >> void

    /** Returns true if we are currently at top-level (i.e., evaluating the main program) */
    def isTopLevel: Boolean = view(component) match {
      case ScMain => true
      case _      => false
    }

    def localCall(component: Component): ScEvalM[PostValue] =
      getStore.flatMap { store =>
        if (!GLOBAL_STORE_ENABLED) {
          val (v, updatedStore) = callLocal(component, store)
          mergeStores(updatedStore).flatMap(_ => result(v))
        } else {
          val v = call(component)
          result(v)
        }
      }

    /**
     * Writes the values of the arguments in the store cache to a designated address.
     * This can be used to determine
     */
    def writeRefinedArguments(): ScEvalM[()] = withContext(context =>
      effectful {
        // TODO: add return value to the value of the opaque result, so that we can differentiate between true and false states
        component match {
          case Call(_, lambda, _) =>
            lambda.variables
              .flatMap(v => context.env.lookup(v.name))
              .flatMap(context.cache.get)
              .filter(v => lattice.isDefinitelyOpq(v._1))
              .map(_._1)
              .zip(0 to lambda.variables.length)
              .foreach { case (value, pos) =>
                writeAddr(OpaqueResultAddr(component, pos, lambda.idn), value)
              }

          case _ => ()
        }
      }
    )

    /** Creates a fresh identifier for the given opaque value */
    def fresh(v: Value): PostValue = if (lattice.isDefinitelyOpq(v)) (v, ScModSemantics.freshIdent) else (v, ScNil())

    def readPure(addr: Addr, storeCache: StoreCache): Value = {
      val (value, _) = merged(read(addr).map(_._1))(Context(env = fnEnv, cache = storeCache, pc = ScNil()))
      value
    }

    // TODO: this is included in the compiler as well, so this can be removed
    def higherOrderToDependentContract(
        domain: ScExp,
        range: ScExp,
        idn: Identity
      ): ScExp =
      ScDependentContract(List(domain), ScLambda(List(ScIdentifier("\"x", Identity.none)), range, range.idn), idn)

    def evalAnd(operands: List[ScExp]): ScEvalM[PostValue] =
      operands match {
        case List(expr) =>
          eval(expr)
        case expr :: exprs =>
          eval(expr).flatMap { value =>
            cond(value, enrichOpaqueInStore(expr, evalAnd(exprs)), result(lattice.schemeLattice.bool(false)))
          }
      }

    def evalCons(car: ScExp, cdr: ScExp): ScEvalM[PostValue] = for {
      evaluatedCar <- eval(car)
      evaluatedCdr <- eval(cdr)
      consValue <- allocCons(evaluatedCar, evaluatedCdr, car.idn, cdr.idn)
    } yield consValue

    def allocCons(
        car: PostValue,
        cdr: PostValue,
        carIdn: Identity,
        cdrIdn: Identity
      ): ScEvalM[PostValue] =
      ??? // TODO: use scheme primitives here

    def evalCar(pai: ScExp): ScEvalM[PostValue] =
      ??? // TODO: use scheme primitives here

    def evalCdr(pai: ScExp): ScEvalM[PostValue] =
      ??? // TODO: use scheme primitives here

    def evalProvideContracts(variables: List[ScIdentifier], contracts: List[ScExp]): ScEvalM[PostValue] =
      sequenceLast(variables.zip(contracts).map { case (variable, contract) =>
        for {
          addr <- lookup(variable.name)
          value <- read(addr)
          // FIXME: this is possibly unsound. It serves as a hack to fix issues with flow insensitivity  and writeForce
          annotatedFn <-
            if (lattice.isDefinitelyArrow(value._1)) pure(value)
            else
              for {
                evaluatedContract <- eval(contract)
                annotatedFn <- applyMon(evaluatedContract, value, contract.idn, variable.idn)
                _ <- writeForce(addr, annotatedFn)
              } yield annotatedFn
        } yield annotatedFn
      })

    def evalDefine(variable: ScIdentifier, expression: ScExp): ScEvalM[PostValue] = for {
      addr <- lookupOrDefine(variable, component)
      value <- eval(expression)
      _ <- write(addr, value)
    } yield value

    def evalDefineFn(
        name: ScIdentifier,
        parameters: List[ScParam],
        body: ScExp,
        idn: Identity
      ): ScEvalM[PostValue] =
      for {
        addr <- lookupOrDefine(name, component)
        lambda <- eval(ScLambda(parameters, body, idn))

        // The logic below for writing the lambda to the store is rather complicated.
        // The reason for this is that the value can be overwritten by a provide/contract,
        // in that case we would like to keep the contract if it points to the lambda,
        // otherwise we use a join.
        value <- read(addr)
        _ <-
          if (lattice.isDefinitelyArrow(value._1) && lattice.getArr(value._1).size == 1) {
            // the address we try to write to contains a contract
            read(lattice.getArr(value._1).head.e).flatMap { (wrappedValue) =>
              if (wrappedValue._1 == lambda._1)
                // the contract wraps us, we don't overwrite (or join)
                unit
              else
                // the contract does not point to us, use a normal join
                write(addr, (lambda._1, name))
            }
          } else
            // the value on the adress is not a contract, use a normal join
            write(addr, (lambda._1, name))

      } yield lambda

    def evalDefineAnnotatedFn(
        name: ScIdentifier,
        parameters: List[ScParam],
        contract: ScExp,
        body: ScExp,
        idn: Identity
      ): ScEvalM[PostValue] =
      for {
        addr <- lookupOrDefine(name, component)
        lambda <- eval(ScLambda(parameters, body, idn))
        evaluatedContract <- eval(contract)
        monitoredFunction <- applyMon(evaluatedContract, lambda, contract.idn, idn)
        _ <- write(addr, (monitoredFunction._1, name))
      } yield monitoredFunction

    def evalProgram(expressions: List[ScExp]): ScEvalM[PostValue] = {
      def addBinding(name: ScIdentifier): ScEvalM[()] =
        addBindingToEnv(name, component) >> lookupOrDefine(name, component) >> unit

      for {
        // extend the environment first
        _ <- sequence(expressions.map {
          case ScDefineAnnotatedFn(name, _, _, _, _) =>
            addBinding(name)

          case ScDefine(name, _, _) =>
            addBinding(name)

          case ScDefineFn(name, _, _, _) =>
            addBinding(name)

          case _ => unit
        })
        // evaluate all expressions in the program
        result <- sequenceLast(expressions.map(eval))
      } yield result
    }

    def evalSet(variable: ScIdentifier, value: ScExp): ScEvalM[PostValue] = for {
      addr <- lookup(variable.name)
      evaluatedValue <- eval(value) // TODO: check if we should not merge states here
      _ <- write(addr, evaluatedValue)
    } yield evaluatedValue

    def evalAssume(
        identifier: ScIdentifier,
        assumption: ScExp,
        expression: ScExp
      ): ScEvalM[PostValue] = ???

    def evalDependentContract(domains: List[ScExp], rangeMaker: ScExp): ScEvalM[PostValue] = {
      val domainAddrs = domains.map(domain => allocGeneric(domain.idn, component))
      val rangeAddr = allocGeneric(rangeMaker.idn, component)
      for {
        evaluatedDomains <- sequence(domains.zip(domainAddrs).map { case (domain, addr) =>
          for {
            evaluated <- eval(domain)
            _ <- write(addr, evaluated)
          } yield addr
        })
        evaluatedRangeMaker <- eval(rangeMaker)
        _ <- write(rangeAddr, evaluatedRangeMaker)
      } yield (lattice.grd(Grd(evaluatedDomains, rangeAddr)), ScNil())
    }

    def evalMon(
        contract: ScExp,
        expression: ScExp,
        identity: Identity
      ): ScEvalM[PostValue] =
      // `mon` has two functions: wrapping a function to monitor it, and checking a flat contract
      for {
        evaluatedContract <- eval(contract)
        evaluatedExpression <- eval(expression)
        res <- applyMon(evaluatedContract, evaluatedExpression, contract.idn, expression.idn)
      } yield res

    def evalLambda(
        params: List[ScParam],
        body: ScExp,
        idn: Identity
      ): ScEvalM[PostValue] = withEnv { env =>
      val clo = Clo(idn, env, params.map(toScIdentifier), ScLambda(params, body, idn), topLevel = isTopLevel)
      ??? // TODO: use scheme lambda representation here?
    //result(lattice.injectClo(clo))
    }

    def evalFlatContract(exp: ScExp): ScEvalM[PostValue] = for {
      evaluatedExp <- eval(exp)
      res <- {
        val addr = allocGeneric(exp.idn, component)
        write(addr, evaluatedExp).flatMap(_ => result(lattice.flat(Flat(addr))))
      }
    } yield res

    def evalLetRec(
        idents: List[ScIdentifier],
        bindings: List[ScExp],
        body: ScExp
      ): ScEvalM[PostValue] =
      for {
        // first evaluate the bindings
        _ <- sequence(idents.lazyZip(bindings).map { (ident, binding) =>
          extended(ident, component) { addr =>
            for {
              evaluatedBinding <- eval(binding)
              _ <- write(addr, evaluatedBinding)
            } yield ()
          }
        })

        // then evaluate the body in an extended environment
        evaluatedBody <- extended(idents, component)(_ => eval(body))
      } yield evaluatedBody

    def evalOpaque(refinements: Set[String]): ScEvalM[PostValue] =
      pure((lattice.opq(Opq(refinements)), ScIdentifier(ScModSemantics.genSym, Identity.none)))

    def evalValue(value: ScValue): ScEvalM[PostValue] = value.value match {
      case ValueInteger(i)   => pure((lattice.schemeLattice.number(i), value))
      case ValueBoolean(b)   => pure((lattice.schemeLattice.bool(b), value))
      case ValueSymbol(s)    => pure((lattice.schemeLattice.symbol(s), ScNil()))
      case ValueReal(r)      => pure((lattice.schemeLattice.real(r), value))
      case ValueCharacter(c) => pure((lattice.schemeLattice.char(c), value))
      case ValueNil          => result(lattice.schemeLattice.nil)
      case ValueString(s)    => pure((lattice.schemeLattice.string(s), ScNil()))
    }

    def evalIdentifier(identifier: ScIdentifier): ScEvalM[PostValue] =
      lookup(identifier.name).flatMap(read)

    def evalSequence(expressions: List[ScExp]): ScEvalM[PostValue] =
      sequence(expressions.map(eval)).map(_.reverse.head)

    def evalFunctionAp(operator: ScExp, operands: List[ScExp]): ScEvalM[PostValue] = for {
      evaluatedOperator <- eval(operator)
      evaluatedOperands <- sequence(operands.map(eval))
      res <- applyFn(evaluatedOperator, evaluatedOperands, operator, operands)
    } yield res

    def evalIf(
        condition: ScExp,
        consequent: ScExp,
        alternative: ScExp
      ): ScEvalM[PostValue] =
      eval(condition).flatMap((value) => conditional(value, condition, consequent, alternative))

    def allocList(values: List[PostValue], idns: List[Identity]): ScEvalM[PostValue] = values match {
      case List() => result(lattice.schemeLattice.nil)
      case v :: values =>
        for {
          cdr <- allocList(values, idns.tail)
          carAddr = allocGeneric(idns.head, component)
          cdrAddr = ScCdrAddr(carAddr)
          _ <- write(carAddr, v)
          _ <- write(cdrAddr, cdr)
        } yield ??? // TODO: use SchemeCons value(lattice.injectCons(Cons(carAddr, cdrAddr)))
    }

    def bindArgs(
        operands: List[PostValue],
        params: List[ScParam],
        syntacticOperands: List[ScExp],
        context: ComponentContext
      ): ScEvalM[()] =
      (operands, params) match {
        case (List(), List()) => unit
        case (values, List(param @ ScVarArgIdentifier(name, idn))) =>
          for {
            listedValues <- allocList(values, syntacticOperands.map(_.idn))
            _ <- write(allocVar(param, context), listedValues)
          } yield ()

        case (value :: values, param :: params) =>
          for {
            _ <- write(allocVar(param, context), value)
            _ <- bindArgs(values, params, syntacticOperands.tail, context)
          } yield ()

        case (_, _) => throw new Exception("Invalid closure application")
      }

    var counter = 0
    def applyFn(
        operator: PostValue,
        operands: List[PostValue],
        syntacticOperator: ScExp,
        syntacticOperands: List[ScExp]
      ): ScEvalM[PostValue] = {

      // we have five distinct cases
      // 1. Primitive application
      // 2. User-defined function application
      // 3. Monitored function (Arr) application
      // 4. Flat contract application
      // 5. Application of an OPQ value

      // 1. Primitive application
      val primitiveAp = lattice.schemeLattice.getPrimitives(operator._1).map { prim =>
        withStoreCacheAdapter { adapter =>
          prim
            .call(syntacticOperator, syntacticOperands.zip(operands.map(_._1)), adapter, this)
            .map { case (value, store) =>
              (value, store.asInstanceOf[StoreCacheAdapter])
            }
            .getOrElse((lattice.bottom, adapter))
        } >>= result _
      }

      // 2. Closure application
      val cloAp =
        lattice
          .getClosure(operator._1)
          .map { clo =>
            for {
              calledComponent <- {
                val context = allocCtx(clo, operands.map(_._1), clo.lambda.idn.pos, component)
                val called = Call(clo.env, clo.lambda, context)
                val calledComponent = newComponent(called)
                bindArgs(operands, clo.lambda.variables, syntacticOperands, context).map(_ => calledComponent)
              }

              value <- localCall(calledComponent)
              // we need to clear out any variables that might have changed that are in our store cache
              // those variables are the variables that are captured by the clojure we just called
              //_ <- evict(clo.capturedVariables)
            } yield value
          }

      // 3. Application of a monitored function (arrow)
      val arrAp = lattice.getArr(operator._1).map { arr =>
        for {
          contract <- options(read(arr.contract).map((c) => lattice.getGrd(c._1)))
          _ <- effectful {
            if (contract.domain.length != operands.length) {
              // TODO: maybe use a blame here instead of crashing the analysis
              throw new Exception("Arity of contract does not match arity of operands in application")
            }
          }

          values <- sequence {
            contract.domain.map(read).zip(operands.zip(syntacticOperands)).map { case (domain, (value, syn)) =>
              domain.flatMap(d => applyMon(d, value, arr.contract.idn, syn.idn))
            }
          }

          rangeMaker <- read(contract.rangeMaker)
          rangeContract <- applyFn(rangeMaker, values, ScNil(), syntacticOperands)
          fn <- read(arr.e)
          resultValue <- applyFn(fn, values, syntacticOperator, syntacticOperands)
          checkedResultValue <- applyMon(rangeContract, resultValue, contract.rangeMaker.idn, arr.e.idn)
        } yield checkedResultValue
      }

      // 4. Flat contract application
      val flatAp = lattice.getFlat(operator._1).map { flat =>
        // TODO: make the flat contract record its results in order for the residuals to be correctly computed
        read(flat.contract).flatMap(value => applyFn(value, operands, syntacticOperator, syntacticOperands))
      }

      // 5. Application of an OPQ value, this yields simply an OPQ value
      val opqAp = lattice.getOpq(operator._1).map { _ =>
        for {
          // TODO: simulate the repeated application of passed lambdas (HAVOC semantics)
          value <- pure((lattice.opq(Opq()), ScModSemantics.freshIdent))
        } yield value
      }

      // 6. Application of thunk
      val thunk = lattice.getThunk(operator._1).map(t => read(t.value))

      for {
        value <- nondets(primitiveAp ++ cloAp ++ arrAp ++ flatAp ++ opqAp ++ thunk)
        // conservatively remove variables from lambdas passed to the called function from the store cache.
        // this is necessary because these lambdas could be applied any number of times by the other functions
        // hence changing the state of the variables stored in the store cache
        _ <- sequence(operands.flatMap((o) => lattice.getClosure(o._1)).map(c => evict(c.capturedVariables)))
      } yield value
    }

    def applyMon(
        evaluatedContract: PostValue,
        evaluatedExpression: PostValue,
        contractIdn: Identity,
        exprIdn: Identity
      ): ScEvalM[PostValue] = {

      // flat contract
      val flatContract = ifFeasible(primProc, evaluatedContract) {
        monFlat(evaluatedContract, evaluatedExpression, exprIdn, contractIdn)
      }

      // dependent contract
      val dependentContract = ifFeasible(primDep, evaluatedContract) {
        val aContract = allocGeneric(contractIdn, component)
        val aExp = allocGeneric(exprIdn, component)
        for {
          _ <- write(aContract, evaluatedContract)
          _ <- write(aExp, evaluatedExpression)
        } yield value(lattice.arr(Arr(contractIdn, exprIdn, aContract, aExp, isTopLevel)))
      }

      nondets(Set(flatContract, dependentContract))
    }

    def monFlat(
        contract: PostValue,
        expressionValue: PostValue,
        blamedIdentity: Identity,
        blamingIdentity: Identity = Identity.none
      ): ScEvalM[PostValue] =
      applyFn(contract,
              List(expressionValue),
              ScNil(),
              List(expressionValue._2)
      ) // TODO: operator is specified to be nil, that might give an issue with store changing flat contracts
        .flatMap { value =>
          cond(value, pure(enrich(contract, expressionValue)), blame(blamedIdentity, blamingIdentity))
        }

    def cond[X](
        condition: PostValue,
        consequent: ScEvalM[X],
        alternative: ScEvalM[X],
        mustReplacePc: Boolean = true
      ): ScEvalM[X] = {
      val t = ifFeasible(primTrue, condition, mustReplacePc)(consequent)
      val f = ifFeasible(primFalse, condition, mustReplacePc)(alternative)
      nondet(t, f)
    }

    def conditional(
        conditionValue: PostValue,
        condition: ScExp,
        consequent: ScExp,
        alternative: ScExp
      ): ScEvalM[PostValue] = {
      val t = enrichOpaqueInStore(condition, eval(consequent))
      cond(conditionValue, t, eval(alternative))
    }

    def enrichOpaqueInStore(condition: ScExp, consequent: => ScEvalM[PostValue]): ScEvalM[PostValue] =
      // enrich the opaque value if it is a simple predicate on an opaque value
      // eg. (mon int? (letrec (y (OPQ int?)) (if (int? x) x y)) is safe
      isPredicateOnVariable(condition) match {
        case Some((operator, variable)) =>
          for {
            opAddr <- lookup(operator)
            varAddr <- lookup(variable)
            opValue <- read(opAddr)
            varValue <- read(varAddr)
            // a writeForce is sound and safe here, because we are either writing the same value to the varAddr, or writing
            // a refined opaque value. Either way, the value on that address still reaches a fixpoint (safety) and is
            // sound because we are not making something more specific which should not be made more specific.
            _ <- writeForce(varAddr, enrich(opValue, varValue))

            // add the constraint symbolicly to the correct variable
            //_ <- effectful {
            //  constrain(varAddr, condition)
            //}

            result <- consequent
          } yield result

        case None => consequent
      }

    def ifFeasible[X](
        op: Prim,
        value: PostValue,
        mustReplacePc: Boolean = true
      )(
        c: => ScEvalM[X]
      ): ScEvalM[X] =
      withPc(feasible(op, value)).flatMap {
        case Some(pc) => if (mustReplacePc) replacePc(pc)(c) else c
        case None     => void
      }

    def guardFeasible(op: Prim, value: PostValue): ScEvalM[()] = ifFeasible(op, value)(pure(()))

    private def feasible(op: Prim, value: PostValue)(pc: PC): Option[PC] =
      value._2 match {
        case _ if !lattice.schemeLattice.isTrue(op.callNoStore(value._1)) =>
          None

        case ScNil(_) => Some(pc)
        case _ =>
          val newPc = pc.and(ScFunctionAp(ScIdentifier(op.name, Identity.none), List(value._2), Identity.none))
          val solver = newSmtSolver(newPc)
          if (solver.isSat) Some(newPc) else None
      }
  }

  def refined(name: String, value: PostValue): PostValue = {
    val refinedValue = lattice
      .getOpq(value._1)
      .map(opq => opq.copy(refinementSet = opq.refinementSet + name))
      .map(lattice.opq)
      .foldLeft(lattice.bottom)((acc, v) => lattice.join(acc, v))

    (refinedValue, value._2)
  }

  def enrich(operator: PostValue, value: PostValue): PostValue = operator._2 match {
    // if we have symbolic information we can enrich the opaque value with this symbolic information,
    case ScIdentifier(name, _) if lattice.isDefinitelyOpq(value._1) && primitives.contains(name) =>
      refined(name, value)

    // if the operator is a primitive, then we can fetch its name from its value
    case _ if lattice.isDefinitelyOpq(value._1) => ??? // TODO lattice.getSymbolic(operator._1).map(refined(_, value)).getOrElse(value)
    case _                                      => value
  }

  def isPredicateOnVariable(expr: ScExp): Option[(String, String)] = expr match {
    case ScFunctionAp(ScIdentifier(operator, _), List(ScIdentifier(variable, _)), _, _) => Some((operator, variable))
    case _                                                                              => None
  }
}
