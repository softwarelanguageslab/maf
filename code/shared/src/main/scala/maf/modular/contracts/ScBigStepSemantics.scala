package maf.modular.contracts
import maf.core.{Environment, Identity, Lattice}
import maf.language.contracts.ScLattice.{Arr, Blame, Clo, Flat, Grd, Opq, Prim}
import maf.language.contracts.{ScExp, _}
import maf.language.sexp.{ValueBoolean, ValueInteger}
import maf.util.benchmarks.Timeout

trait ScBigStepSemantics extends ScModSemantics with ScPrimitives {
  type PC = ScExp
  type PostValue = (Value, ScExp)
  type StoreCache = Map[Addr, PostValue]
  case class Context(env: Environment[Addr], pc: PC, cache: StoreCache)

  private val primTrue  = ScLattice.Prim("true?")
  private val primFalse = ScLattice.Prim("false?")
  private val primProc  = ScLattice.Prim("proc?")
  private val primDep   = ScLattice.Prim("dependent-contract?")

  def value(v: Value): PostValue = (v, ScNil())

  object ScEvalM {
    def pure[X](v: => X): ScEvalM[X] = ScEvalM((context) => List((context, v)).toSet)
    def unit: ScEvalM[()] = pure(())
    def void[X]: ScEvalM[X] = ScEvalM((context) => Set[(Context, X)]())

    case class ScEvalM[X](run: Context => Set[(Context, X)]) {
      def map[Y](f: X => Y): ScEvalM[Y] = ScEvalM((context) => {
        run(context).map {
          case (updatedContext, value) => (updatedContext, f(value))
        }
      })

      def flatMap[Y](f: X => ScEvalM[Y]): ScEvalM[Y] = ScEvalM((context) =>
        run(context).flatMap {
          case (updatedContext, value) => f(value).run(updatedContext)
        }
      )
    }

    def sequence[X](xs: List[ScEvalM[X]]): ScEvalM[List[X]] = xs match {
      case List() => pure(List())
      case _ =>
        for {
          result <- xs.head
          results <- sequence(xs.tail)
        } yield (result :: results)
    }

    def withEnv[B](f: Environment[Addr] => ScEvalM[B]): ScEvalM[B] =
        ScEvalM((context) => f(context.env).run(context))

    def lookup(identifier: String): ScEvalM[Addr] = withEnv((env) => {
      pure(env.lookup(identifier).getOrElse(throw new Exception(s"variable ${identifier} not found")))
    })

    def nondet[X](t: ScEvalM[X], f: ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
        val resF = f.run(context)
        val resT = t.run(context)
        resF ++ resT
    })

    def nondets[X](s: Set[ScEvalM[X]]): ScEvalM[X] = ScEvalM((context) => {
      s.flatMap(_.run(context))
    })

    def withPc[X](f: PC => X): ScEvalM[X] = ScEvalM((context) => {
      Set((context, f(context.pc)))
    })

    def withStoreCache[X](f: StoreCache => ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      f(context.cache).run(context)
    })

    def addToCache(mapping: (Addr, PostValue)): ScEvalM[()] = ScEvalM((context) => {
      List((context.copy(cache = context.cache + mapping), ())).toSet
    })

    def replacePc[X](pc: PC)(c: ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      c.run(context.copy(pc = pc))
    })

    /**
      * This function creates a computation that yields a single state contain the abstract value with no
      * symbolic information.
      */
    def result(v: Value): ScEvalM[PostValue] = pure(value(v))

    def extended[X](ident: ScIdentifier, component: Component)(c: Addr => ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      val addr = allocVar(ident, component)
      val extendedEnv = context.env.extend(ident.name, addr)
      c(addr).run(context.copy(env = extendedEnv)).map {
        case (updatedContext, value) => (updatedContext.copy(env = context.env), value)
      }
    })

    /**
      * Given a computation that yields a value corresponding to a certain lattice, this function runs the computation
      * on the given context, and joins all the values of the resulting states together using the join operator of the
      * lattice.
      */
    def merged[L: Lattice](c: ScEvalM[L])(context: Context): L = {
      c.run(context).foldLeft(Lattice[L].bottom)((acc, v) => v match {
        case (_, l) => Lattice[L].join(acc, l)
      })
    }

    /**
      * Given a computation that yields states that contain sets of values, this operator yields a single computation
      * that gives rises to a state for every element in the given set.
      */
    def options[X](c: ScEvalM[Set[X]]): ScEvalM[X] = ScEvalM((context) =>
      c.run(context).flatMap {
        case (updatedContext, set) => set.map((updatedContext, _))
      }
    )

    def debug(c: => ()): ScEvalM[()] = unit.flatMap(_ => {
      c
      pure(())
    })

    def trace[X]: (X => ScEvalM[X]) = { x =>
      println("trace", x)
      pure(x)
    }

    def evict(addresses: List[Addr]): ScEvalM[()] = ScEvalM(context => {
      Set((context.copy(cache = context.cache.removedAll(addresses)), ()))
    })
  }

  import ScEvalM._

  trait IntraScBigStepSemantics extends IntraScAnalysis {
    /**
      * Read a value from the store cache, if it is not in the store cache, retrieve it from the global store
      * @param addr the addres to read from
      * @return a value combined with (optionally) its symbolic component
      */
    def read(addr: Addr): ScEvalM[PostValue] = withStoreCache((cache) => {
      cache.get(addr) match {
        case Some(value) => pure(value)
        case None => result(readAddr(addr))
      }
    })

    /**
      * Write a value to both the store cache and the global store
      * @param addr the address to write the value to
      * @param value the value to write
      */
    def write(addr: Addr, value: PostValue): ScEvalM[()] = {
      writeAddr(addr, value._1)
      addToCache(addr -> value)
    }

    def blame[X](blamedIdentity: Identity, blamingIdentity: Identity = Identity.none): ScEvalM[X]  =
      unit.flatMap(_ => {
        writeBlame(Blame(blamedIdentity, blamingIdentity))
        void
      })

    /**
      * Compute the context of the current component
      * @return a new context based on the environment of the component under analysis
      */
    def initialContext: Context = {
      var storeCache = Map[Addr, PostValue]()
      primBindings.foreach {
        case (name, addr) =>
          storeCache = storeCache + (addr -> (lattice.injectPrim(Prim(name)), ScIdentifier(name, Identity.none)))
      }

      fnEnv.content.values.foreach((addr) => {
        val value = readAddr(addr)
        if (lattice.isDefinitelyOpq(value)) {
          storeCache += addr -> (value, ScIdentifier(ScModSemantics.genSym, Identity.none))
        }
      })

      Context(env = fnEnv, cache = storeCache, pc = ScNil())
    }

    def analyze(_ignored_timeout: Timeout.T): Unit = {
      val value = merged(eval(fnBody).map(_._1))(initialContext)
      writeResult(value, component)
    }

    def eval(expr: ScExp): ScEvalM[PostValue] = expr match {
      case ScBegin(expressions, _) => evalSequence(expressions)
      case ScIf(condition, consequent, alternative, _) => evalIf(condition, consequent, alternative)
      case ScLetRec(ident, binding, body, _) => evalLetRec(ident, binding, body)
      case ScRaise(error, _) => ???
      case ScSet(variable, value, _) => evalSet(variable, value)
      case ScFunctionAp(operator, operands, _) => evalFunctionAp(operator, operands)
      case v: ScValue => evalValue(v)
      case exp: ScIdentifier => evalIdentifier(exp)
      case ScMon(contract, expression, idn) => evalMon(contract, expression, idn)
      case ScOpaque(_, refinements) => evalOpaque(refinements)
      case ScHigherOrderContract(domain, range, idn) => eval(higherOrderToDependentContract(domain, range, idn))
      case ScDependentContract(domain, rangeMaker, _) => evalDependentContract(domain, rangeMaker)
      case ScFlatContract(expression, _) => evalFlatContract(expression)
      case ScLambda(params, body, idn) => evalLambda(params, body, idn)
    }

    def higherOrderToDependentContract(domain: ScExp, range: ScExp, idn: Identity): ScExp =
      ScDependentContract(domain, ScLambda(List(ScIdentifier("\"x", Identity.none)), range, range.idn), idn)

    def evalSet(variable: ScIdentifier, value: ScExp): ScEvalM[PostValue] = for {
      addr <- lookup(variable.name)
      evaluatedValue <- eval(value) // TODO: check if we should not merge states here
      _ <- write(addr, evaluatedValue)
    } yield evaluatedValue

    def evalDependentContract(domain: ScExp, rangeMaker: ScExp): ScEvalM[PostValue] = {
      val domainAddr = allocGeneric(domain.idn, component)
      val rangeAddr  = allocGeneric(rangeMaker.idn, component)
      for {
        evaluatedDomain <- eval(domain)
        evaluatedRangeMaker <- eval(rangeMaker)
        _ <- write(domainAddr, evaluatedDomain)
        _ <- write(rangeAddr, evaluatedRangeMaker)
      } yield (lattice.injectGrd(Grd(domainAddr, rangeAddr)), ScNil())
    }

    def evalMon(contract: ScExp, expression: ScExp, identity: Identity): ScEvalM[PostValue] = {
      // `mon` has two functions: wrapping a function to monitor it, and checking a flat contract
      for {
        evaluatedContract <- eval(contract)
        evaluatedExpression <- eval(expression)
        res <- applyMon(evaluatedContract, evaluatedExpression, contract.idn, expression.idn)
      } yield res
    }

    def evalLambda(params: List[ScIdentifier], body: ScExp, idn: Identity): ScEvalM[PostValue] = withEnv { env =>
      result(lattice.injectClo(Clo(idn, env, params, ScLambda(params, body, idn))))
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
      eval(condition).flatMap((value) => conditional(value, consequent, alternative))

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
        val context         = allocCtx(clo, operands.map(_._1), clo.lambda.idn.pos, component)
        val called          = Call(clo.env, clo.lambda, context)
        val calledComponent = newComponent(called)
        operands.zip(clo.lambda.variables.map(v => allocVar(v, calledComponent))).foreach {
          case (value, addr) =>
            writeAddr(addr, value._1)
        }

        for {
          value <- result(call(calledComponent))
          // we need to clear out any variables that might have changed that are in our store cache
          // those variables are the variables that are captured by the clojure we just called
          _ <- evict(clo.capturedVariables)
        } yield value
      }

      // 3. Application of a monitored function (arrow)
      val arrAp = lattice.getArr(operator._1).map { arr =>
        for {
          contract <- options(read(arr.contract).map((c) => lattice.getGrd(c._1)))
          domain <- read(contract.domain)
          rangeMaker <- read(contract.rangeMaker)
          value <- applyMon(domain, operands.head, arr.contract.idn, syntacticOperands.head.idn)
          rangeContract <- applyFn(rangeMaker, List(value), List(syntacticOperands.head))
          fn <- read(arr.e)
          resultValue <- applyFn(fn, List(value), List(syntacticOperands.head))
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

      for {
        value <- nondets(primitiveAp ++ cloAp ++ arrAp ++ flatAp ++ opqAp)
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
        } yield value(lattice.injectArr(Arr(contractIdn, exprIdn, aContract, aExp)))
      }

      nondets(Set(flatContract, dependentContract))
    }

    def monFlat(contract: PostValue, expressionValue: PostValue, blamedIdentity: Identity): ScEvalM[PostValue] =
      applyFn(contract, List(expressionValue))
        .flatMap(value => cond(value, pure(enrich(contract, expressionValue)), blame(blamedIdentity)))

    def cond[X](condition: PostValue, consequent: ScEvalM[X], alternative: ScEvalM[X]): ScEvalM[X] = {
      val t = ifFeasible(primTrue, condition) { consequent }
      val f = ifFeasible(primFalse, condition) { alternative }
      nondet(t, f)
    }

    def conditional(condition: PostValue, consequent: ScExp, alternative: ScExp): ScEvalM[PostValue] =
      cond(condition, eval(consequent), eval(alternative))

    def ifFeasible[X](op: Prim, value: PostValue)(c: => ScEvalM[X]): ScEvalM[X] =
      withPc(feasible(op, value)).flatMap {
        case Some(pc) => replacePc(pc)(c)
        case None => void
      }

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
        case _ => lattice.getSymbolic(operator._1).map(refined(_, value)).getOrElse(value)
      }
}
