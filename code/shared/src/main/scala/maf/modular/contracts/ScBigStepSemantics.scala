package maf.modular.contracts
import maf.core.{Environment, Identity, Lattice}
import maf.language.contracts.ScLattice._
import maf.language.contracts.{ScExp, _}
import maf.language.sexp.{ValueBoolean, ValueInteger}
import maf.util.benchmarks.Timeout

trait ScBigStepSemantics extends ScModSemantics with ScPrimitives with ScSemanticsMonad {
  private val primTrue  = ScLattice.Prim("true?")
  private val primFalse = ScLattice.Prim("false?")
  private val primProc  = ScLattice.Prim("proc?")
  private val primDep   = ScLattice.Prim("dependent-contract?")
  private var totalRuns = 0

  import ScEvalM._

  trait IntraScBigStepSemantics extends IntraScAnalysis {
    /**
      * Compute the context of the current component
      * @return a new context based on the environment of the component under analysis
      */
    def initialContext: Context = {
      var storeCache: StoreCache = componentStore.view.mapValues(v => (v, ScNil())).toMap
      primBindings.foreach {
        case (name, addr) =>
          val value = storeCache(addr)._1
          storeCache = storeCache +  (addr -> ((value, ScIdentifier(name, Identity.none))))
          storeCache = storeCache + (ScPrimAddr(name) -> (lattice.injectPrim(Prim(name)), ScIdentifier(name, Identity.none)))
      }

      fnEnv.mapAddrs((addr) => {
        val value = readPure(addr, storeCache)
        if (lattice.isDefinitelyOpq(value)) {
          storeCache += (addr -> (value, ScIdentifier(ScModSemantics.genSym, Identity.none)))
        }
        addr
      })

      Context(env = fnEnv, cache = storeCache, pc = ScNil())
    }

    def analyze(_ignored_timeout: Timeout.T): Unit = {
      totalRuns += 1
      if (totalRuns > 10) {
        throw new Exception("Timeout exceeded")
      }

      println("================================")
      val (value, store) = merged(eval(fnBody).map(_._1))(initialContext)
      println(s"Return value is $value")
      writeReturnStore(store)
      writeResult(value, component)
    }

    def eval(expr: ScExp): ScEvalM[PostValue] = expr match {
      case ScBegin(expressions, _) => evalSequence(expressions)
      case ScIf(condition, consequent, alternative, _) => evalIf(condition, consequent, alternative)
      case ScLetRec(ident, binding, body, _) => evalLetRec(ident, binding, body)
      case ScRaise(_, _) => ???
      case ScSet(variable, value, _) => evalSet(variable, value)
      case ScFunctionAp(operator, operands, _, _) => evalFunctionAp(operator, operands)
      case v: ScValue => evalValue(v)
      case exp: ScIdentifier => evalIdentifier(exp)
      case ScMon(contract, expression, idn, _) => evalMon(contract, expression, idn)
      case ScOpaque(_, refinements) => evalOpaque(refinements)
      case ScHigherOrderContract(domain, range, idn) => eval(higherOrderToDependentContract(domain, range, idn))
      case ScDependentContract(domain, rangeMaker, _) => evalDependentContract(domain, rangeMaker)
      case ScFlatContract(expression, _) => evalFlatContract(expression)
      case ScLambda(params, body, idn) => evalLambda(params, body, idn)
      case ScAssume(identifier, assumption, expression, _) => evalAssume(identifier, assumption, expression)
      case ScProgram(expressions, _) => evalProgram(expressions)
      case ScDefine(variable, expression, _) => evalDefine(variable, expression)
      case ScDefineFn(name, parameters, body, idn) => evalDefineFn(name, parameters, body, idn)
      case ScDefineAnnotatedFn(name, parameters, contract, expression, idn) =>
        evalDefineAnnotatedFn(name, parameters, contract, expression, idn)
      case ScProvideContracts(variables, contracts, _) => evalProvideContracts(variables, contracts)
      case ScCons(car, cdr, _) => evalCons(car, cdr)
      case ScCar(pai, _) => evalCar(pai)
      case ScCdr(pai, _) => evalCdr(pai)
    }

    /**
      * Read a value from the store cache, if it is not in the store cache, retrieve it from the global store
      * @param addr the addres to read from
      * @return a value combined with (optionally) its symbolic component
      */
    def read(addr: Addr): ScEvalM[PostValue] = withStoreCache((cache) => {
      cache.get(addr) match {
        case Some(value) => pure(value)
        case None if GLOBAL_STORE_ENABLED=> result(readAddr(addr))
        // if we are not using a global store, then we throw an exception
        // this should never occur, but if it does, it is much easier to track down.
        case None =>
          throw new Exception(s"Addr $addr is not found in store $cache while analysing $component")
      }
    })

    /**
      * Write a value to both the store cache and the global store
      * @param addr the address to write the value to
      * @param value the value to write
      */
    def write(addr: Addr, value: PostValue): ScEvalM[()] = {
      for {
        _ <- effectful { if (GLOBAL_STORE_ENABLED) writeAddr(addr, value._1) }
        _ <- writeLocal(addr, value)
      } yield ()
    }

    def writeForce(addr: Addr, value: PostValue): ScEvalM[()] = {
      for {
        _ <- effectful { if (GLOBAL_STORE_ENABLED) forceWrite(addr, value._1) }
        _ <- writeLocalForce(addr, value)
      } yield ()
    }

    def writeLocal(addr: Addr, value: PostValue): ScEvalM[()] = {
      joinInCache(addr, value)
    }

    def writeLocalForce(addr: Addr, value: PostValue): ScEvalM[()] = {
      addToCache(addr -> value)
    }

    def blame[X](blamedIdentity: Identity, blamingIdentity: Identity = Identity.none): ScEvalM[X]  =
      unit.flatMap(_ => {
        writeBlame(Blame(blamedIdentity, blamingIdentity))
        void
      })

    /**
      * Returns true if we are currently at top-level (i.e., evaluating the main program)
      */
    def isTopLevel: Boolean = view(component) match {
      case ScMain => true
      case _ => false
    }

    def localCall(component: Component): ScEvalM[PostValue] =
      getStore.flatMap(store => {
        if (!GLOBAL_STORE_ENABLED) {
          val (v, updatedStore) = callLocal(component, store)
          mergeStores(updatedStore).flatMap(_ => result(v))
        } else {
          val v = call(component)
          result(v)
        }
      })

    /**
      * Writes the values of the arguments in the store cache to a designated address.
      * This can be used to determine
      */
    def writeRefinedArguments(): ScEvalM[()] = withContext(context => effectful {
        // TODO: add return value to the value of the opaque result, so that we can differentiate between true and false states
        component match {
          case Call(_, lambda, _) =>
            lambda
              .variables
              .flatMap(v => context.env.lookup(v.name))
              .flatMap(context.cache.get)
              .filter(v => lattice.isDefinitelyOpq(v._1))
              .map(_._1)
              .zip(0 to lambda.variables.length)
              .foreach {
                case (value, pos) => writeAddr(OpaqueResultAddr(component, pos, lambda.idn), value)
              }

          case _ => ()
        }
    })

    /**
      * Creates a fresh identifier for the given opaque value
      */
    def fresh(v: Value): PostValue =  if (lattice.isDefinitelyOpq(v)) (v, ScModSemantics.freshIdent) else (v, ScNil())

    def readPure(addr: Addr, storeCache: StoreCache): Value = {
      val (value, _) = merged(read(addr).map(_._1))(Context(env = fnEnv, cache = storeCache, pc = ScNil()))
      value
    }

    def higherOrderToDependentContract(domain: ScExp, range: ScExp, idn: Identity): ScExp =
      ScDependentContract(domain, ScLambda(List(ScIdentifier("\"x", Identity.none)), range, range.idn), idn)

    def evalCons(car: ScExp, cdr: ScExp): ScEvalM[PostValue] = for {
      evaluatedCar <- eval(car)
      evaluatedCdr <- eval(cdr)
      consValue <- allocCons(evaluatedCar, evaluatedCdr, car.idn, cdr.idn)
    } yield consValue

    def allocCons(car: PostValue, cdr: PostValue, carIdn: Identity, cdrIdn: Identity): ScEvalM[PostValue] = for {
      _ <- unit
      carAddr = allocGeneric(carIdn, component)
      cdrAddr = allocGeneric(cdrIdn, component)
      _ <- write(carAddr, car)
      _ <- write(cdrAddr, cdr)
    } yield value(lattice.injectCons(Cons(carAddr, cdrAddr)))

    def evalCar(pai: ScExp): ScEvalM[PostValue] =
      eval(pai).flatMap((pai) => {
        val topValue = if (lattice.top == pai) { Set(result(lattice.top)) } else { Set() }
        nondets(lattice.getCons(pai._1).map(p => read(p.car)) ++ topValue)
      })

    def evalCdr(pai: ScExp): ScEvalM[PostValue] =
      eval(pai).flatMap((pai) => {
        val topValue = if (lattice.top == pai) { Set(result(lattice.top)) } else { Set() }
        nondets(lattice.getCons(pai._1).map(p => read(p.cdr)) ++ topValue)
      })


    def evalProvideContracts(variables: List[ScIdentifier], contracts: List[ScExp]): ScEvalM[PostValue] =
      sequenceLast(variables.zip(contracts).map {
        case (variable, contract) => for {
          addr <- lookup(variable.name)
          value <- read(addr)
          evaluatedContract <- eval(contract)
          annotatedFn <- applyMon(evaluatedContract, value, contract.idn, variable.idn)
          _ <- writeForce(addr, annotatedFn)
        } yield annotatedFn
      })

    def evalDefine(variable: ScIdentifier, expression: ScExp): ScEvalM[PostValue] = for {
      addr <- lookupOrDefine(variable, component)
      value <- eval(expression)
      _ <- write(addr, value)
    } yield value

    def evalDefineFn(name: ScIdentifier, parameters: List[ScParam], body: ScExp, idn: Identity): ScEvalM[PostValue] =
      for {
        addr <- lookupOrDefine(name, component)
        lambda <- eval(ScLambda(parameters, body, idn))
        _ <- write(addr, lambda)
      } yield lambda

    def evalDefineAnnotatedFn(name: ScIdentifier, parameters: List[ScParam], contract: ScExp, body: ScExp, idn: Identity): ScEvalM[PostValue] =
      for {
        addr <- lookupOrDefine(name, component)
        lambda <- eval(ScLambda(parameters, body, idn))
        evaluatedContract <- eval(contract)
        monitoredFunction <- applyMon(evaluatedContract, lambda, contract.idn, idn)
        _ <- write(addr, monitoredFunction)
      } yield monitoredFunction

    def evalProgram(expressions: List[ScExp]): ScEvalM[PostValue] = {
      def addBinding(name: ScIdentifier): ScEvalM[()] = addBindingToEnv(name, component)

      for {
        // extend the environment first
        _ <- sequence(expressions.map {
          case ScDefineAnnotatedFn(name, _, _, _, _) => addBinding(name)
          case ScDefine(name, _, _) => addBinding(name)
          case ScDefineFn(name, _, _, _) => addBinding(name)
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

    def evalAssume(identifier: ScIdentifier, assumption: ScExp, expression: ScExp): ScEvalM[PostValue] = {
      for {
        evaluatedAssumption <- eval(assumption)
        evaluatedExpression <- eval(expression)
        _ <- guardFeasible(primProc, evaluatedAssumption)
        identifierValueAddr <- lookup(identifier.name)
        identifierValue <- read(identifierValueAddr)
        result <- applyFn(evaluatedAssumption, List(identifierValue))
        // TODO: see if it is possible to extend the path condition
        // TODO: not sure what to do here if the value already was opaque with refinements
        // TODO: we should actually only do this when the value of the assumption is Top or Opq
        _ <- writeForce(identifierValueAddr, enrich(evaluatedAssumption, fresh(lattice.injectOpq(Opq()))))
      } yield evaluatedExpression
    }

    def evalDependentContract(domain: ScExp, rangeMaker: ScExp): ScEvalM[PostValue] = {
      val domainAddr = allocGeneric(domain.idn, component)
      val rangeAddr  = allocGeneric(rangeMaker.idn, component)
      for {
        evaluatedDomain <- eval(domain)
        evaluatedRangeMaker <- eval(rangeMaker)
        _ <- write(domainAddr, evaluatedDomain)
        _ <- write(rangeAddr, evaluatedRangeMaker)
      } yield (lattice.injectGrd(Grd(List(domainAddr), rangeAddr)), ScNil())
    }

    def evalMon(contract: ScExp, expression: ScExp, identity: Identity): ScEvalM[PostValue] = {
      // `mon` has two functions: wrapping a function to monitor it, and checking a flat contract
      for {
        evaluatedContract <- eval(contract)
        evaluatedExpression <- eval(expression)
        res <- applyMon(evaluatedContract, evaluatedExpression, contract.idn, expression.idn)
      } yield res
    }

    def evalLambda(params: List[ScParam], body: ScExp, idn: Identity): ScEvalM[PostValue] = withEnv { env =>
      val clo = Clo(idn, env, params.map(toScIdentifier), ScLambda(params, body, idn), topLevel = isTopLevel)
      result(lattice.injectClo(clo))
    }

    def evalFlatContract(exp: ScExp): ScEvalM[PostValue] = for {
      evaluatedExp <- eval(exp)
      res <- {
        val addr = allocGeneric(exp.idn, component)
        write(addr, evaluatedExp).flatMap(_ => result(lattice.injectFlat(Flat(addr))))
      }
    } yield res

    def evalLetRec(ident: ScIdentifier, binding: ScExp, body: ScExp): ScEvalM[PostValue] = for {
      evaluatedBody <- extended(ident, component) { addr => for {
          evaluatedBinding <- eval(binding)
          _ <- write(addr, evaluatedBinding)
          evaluatedBody <- eval (body)
        } yield evaluatedBody
      }
    } yield evaluatedBody

    def evalOpaque(refinements: Set[String]): ScEvalM[PostValue] =
      pure((lattice.injectOpq(Opq(refinements)), ScIdentifier(ScModSemantics.genSym, Identity.none)))

    def evalValue(value: ScValue): ScEvalM[PostValue] = value.value match {
      case ValueInteger(i) => pure((lattice.injectInteger(i), value))
      case ValueBoolean(b) => pure((lattice.injectBoolean(b), value))
      case _ => throw new Exception("unsupported value")
    }

    def evalIdentifier(identifier: ScIdentifier): ScEvalM[PostValue] =
      lookup(identifier.name).flatMap(read)

    def evalSequence(expressions: List[ScExp]): ScEvalM[PostValue] =
      sequence(expressions.map(eval)).map(_.reverse.head)

    def evalFunctionAp(operator: ScExp, operands: List[ScExp]): ScEvalM[PostValue] = for {
      evaluatedOperator <- eval(operator)
      evaluatedOperands <- sequence(operands.map(eval))
      res <- applyFn(evaluatedOperator, evaluatedOperands, operands)
    } yield res

    def evalIf(condition: ScExp, consequent: ScExp, alternative: ScExp): ScEvalM[PostValue] =
      eval(condition).flatMap((value) => conditional(value, condition, consequent, alternative))

    def allocList(values: List[PostValue], idns: List[Identity]): ScEvalM[PostValue] = values match {
      case List() => result(lattice.injectNil)
      case v :: values => for {
        cdr <- allocList(values, idns.tail)
        carAddr = allocGeneric(idns.head, component)
        cdrAddr = ScCdrAddr(carAddr)
        _ <- write(carAddr, v)
        _ <- write(cdrAddr, cdr)
      } yield value(lattice.injectCons(Cons(carAddr, cdrAddr)))
    }

    def bindArgs(operands: List[PostValue], params: List[ScParam], syntacticOperands: List[ScExp], context: ComponentContext): ScEvalM[()] =
      (operands, params) match {
        case (List(), List()) => unit
        case (values, List(param@ScVarArgIdentifier(name, idn))) =>
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

    def applyFn(operator: PostValue, operands: List[PostValue], syntacticOperands: List[ScExp] = List()): ScEvalM[PostValue] = {
      // we have three distinct cases
      // 1. Primitive application
      // 2. User-defined function application
      // 3. Monitored function (Arr) application
      // 4. Flat contract application
      // 5. Application of an OPQ value

      // 1. Primitive application
      val primitiveAp = lattice.getPrim(operator._1).map { prim =>
        pure((lattice.applyPrimitive(prim)(operands.map(_._1) : _*), operator._2.app(operands.map(_._2))))
      }

      // 2. Closure application
      val cloAp = lattice.getClo(operator._1).map { clo =>
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
          values <- sequence {
            contract.domain.map(read).zip(operands.zip(syntacticOperands)).map {
              case (domain, (value, syn)) =>
                domain.flatMap(d => applyMon(d, value, arr.contract.idn, syn.idn))
            }
          }

          rangeMaker <- read(contract.rangeMaker)
          rangeContract <- applyFn(rangeMaker, values, syntacticOperands)
          fn <- read(arr.e)
          resultValue <- applyFn(fn, values, syntacticOperands)
          checkedResultValue <- applyMon(rangeContract, resultValue, contract.rangeMaker.idn, arr.e.idn)
        } yield checkedResultValue
      }

      // 4. Flat contract application
      val flatAp = lattice.getFlat(operator._1).map { flat =>
        // TODO: make the flat contract record its results in order for the residuals to be correctly computed
        read(flat.contract).flatMap(value => applyFn(value, operands, syntacticOperands))
      }

      // 5. Application of an OPQ value, this yields simply an OPQ value
      val opqAp = lattice.getOpq(operator._1).map { _ =>
        for {
          // TODO: simulate the repeated application of passed lambdas (HAVOC semantics)
          value <- pure((lattice.injectOpq(Opq()), ScModSemantics.freshIdent))
        } yield value
      }

      // 6. Application of thunk
      val thunk = lattice.getThunk(operator._1).map( t =>
        read(t.value)
      )

      for {
        value <- nondets(primitiveAp ++ cloAp ++ arrAp ++ flatAp ++ opqAp ++ thunk)
        // conservatively remove variables from lambdas passed to the called function from the store cache.
        // this is necessary because these lambdas could be applied any number of times by the other functions
        // hence changing the state of the variables stored in the store cache
        _ <- sequence(operands.flatMap((o) => lattice.getClo(o._1)).map(c => evict(c.capturedVariables)))
      } yield value
    }

    def applyMon(evaluatedContract: PostValue,
                 evaluatedExpression: PostValue,
                 contractIdn: Identity,
                 exprIdn: Identity): ScEvalM[PostValue] = {
      // flat contract
      val flatContract = ifFeasible(primProc, evaluatedContract) {
        monFlat(evaluatedContract, evaluatedExpression, exprIdn)
      }

      // dependent contract
      val dependentContract = ifFeasible(primDep, evaluatedContract) {
        val aContract = allocGeneric(contractIdn, component)
        val aExp = allocGeneric(exprIdn, component)
        for {
          _ <- write(aContract, evaluatedContract)
          _ <- write(aExp, evaluatedExpression)
        } yield value(lattice.injectArr(Arr(contractIdn, exprIdn, aContract, aExp, isTopLevel)))
      }

      nondets(Set(flatContract, dependentContract))
    }

    def monFlat(contract: PostValue, expressionValue: PostValue, blamedIdentity: Identity): ScEvalM[PostValue] =
      applyFn(contract, List(expressionValue), List(expressionValue._2))
        .flatMap(value => {
          cond(value, pure(enrich(contract, expressionValue)), blame(blamedIdentity))
        })

    def cond[X](condition: PostValue, consequent: ScEvalM[X], alternative: ScEvalM[X], mustReplacePc: Boolean = true): ScEvalM[X] = {
      val t = ifFeasible(primTrue, condition, mustReplacePc) { consequent }
      val f = ifFeasible(primFalse, condition, mustReplacePc) { alternative }
      nondet(t, f)
    }

    def conditional(conditionValue: PostValue, condition: ScExp, consequent: ScExp, alternative: ScExp): ScEvalM[PostValue] = {
      // enrich the opaque value if it is a simple predicate on an opaque value
      // eg. (mon int? (letrec (y (OPQ int?)) (if (int? x) x y)) is safe
      val t = isPredicateOnVariable(condition) match {
        case Some((operator, variable)) => for {
          opAddr <- lookup(operator)
          varAddr <- lookup(variable)
          opValue <- read(opAddr)
          varValue <- read(varAddr)
          // a writeForce is sound and safe here, because we are either writing the same value to the varAddr, or writing
          // a refined opaque value. Either way, the value on that address still reaches a fixpoint (safety) and is
          // sound because we are not making something more specific which should not be made more specific.
          _ <- writeForce(varAddr, enrich(opValue, varValue))
          result <- eval(consequent)
        } yield result
        case None => eval(consequent)
      }

      cond(conditionValue, t, eval(alternative))
    }

    def ifFeasible[X](op: Prim, value: PostValue, mustReplacePc: Boolean = true)(c: => ScEvalM[X]): ScEvalM[X] =
      withPc(feasible(op, value)).flatMap {
        case Some(pc) => if (mustReplacePc) replacePc(pc)(c) else c
        case None => void
      }

    def guardFeasible(op: Prim, value: PostValue): ScEvalM[()] = ifFeasible(op, value)(pure(()))

    private def feasible(op: Prim, value: PostValue)(pc: PC): Option[PC] =
      value._2 match {
        case _ if !lattice.isTrue(lattice.applyPrimitive(op)(value._1)) => None
        case ScNil(_) => Some(pc)
        case _ =>
          val newPc = pc.and(ScFunctionAp(ScIdentifier(op.operation, Identity.none), List(value._2), Identity.none))
          val solver = newSmtSolver(newPc)
          if (solver.isSat) Some(newPc) else None
      }
  }

  def refined(name: String, value: PostValue): PostValue = {
    val refinedValue = lattice.getOpq(value._1)
      .map(opq => opq.copy(refinementSet = opq.refinementSet + name))
      .map(lattice.injectOpq)
      .foldLeft(lattice.bottom)((acc, v) => lattice.join(acc, v))

    (refinedValue, value._2)
  }

  def enrich(operator: PostValue, value: PostValue): PostValue = operator._2 match {
        // if we have symbolic information we can enrich the opaque value with this symbolic information,
        case ScIdentifier(name, _) if lattice.isDefinitelyOpq(value._1) && primitives.contains(name) =>
          refined(name, value)

        // if the operator is a primitive, then we can fetch its name from its value
        case _ if lattice.isDefinitelyOpq(value._1) => lattice.getSymbolic(operator._1).map(refined(_, value)).getOrElse(value)
        case _ => value
      }


  def isPredicateOnVariable(expr: ScExp): Option[(String, String)] = expr match {
    case ScFunctionAp(ScIdentifier(operator, _), List(ScIdentifier(variable, _)), _, _) => Some((operator, variable))
    case _ => None
  }
}