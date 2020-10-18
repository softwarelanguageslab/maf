package maf.modular.contracts
import maf.core.{Environment, Identity}
import maf.language.contracts.ScLattice.{Flat, Opq, Prim}
import maf.language.contracts.{ScExp, _}
import maf.language.sexp.{ValueBoolean, ValueInteger}

trait ScBigStepSemantics extends ScModSemantics {
  type PC = ScExp
  type PostValue = (Value, ScExp)
  type StoreCache = Map[Addr, PostValue]
  case class Context(env: Environment[Addr], pc: PC, cache: StoreCache)
  private val primTrue  = ScLattice.Prim("true?")
  private val primFalse = ScLattice.Prim("false?")

  def value(v: Value): PostValue = (v, ScNil())

  object ScEvalM {
    def pure[X](v: X): ScEvalM[X] = ScEvalM((context) => List((context, v)).toSet)
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

    def sequence[X](xs: List[ScEvalM[X]]): ScEvalM[List[X]] = ???

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

    def withPc[X](f: PC => X): ScEvalM[X] = ScEvalM((context) => {
      Set((context, f(context.pc)))
    })

    def replacePc(pc: PC): ScEvalM[()] = ScEvalM((context) => {
      Set((context.copy(pc = pc), ()))
    })

    def read(addr: Addr): ScEvalM[PostValue] = ???
    def write(addr: Addr, value: PostValue): ScEvalM[()] = ???

    def result(v: Value) = pure(value(v))

    def extended[X](ident: ScIdentifier, component: Component)(c: Addr => ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      val addr = allocVar(ident, component)
      val extendedEnv = context.env.extend(ident.name, addr)
      c(addr).run(context.copy(env = extendedEnv))
    })
  }

  import ScEvalM._

  trait IntraScBigStepSemantics extends IntraScAnalysis {
    def eval(expr: ScExp): ScEvalM[PostValue] = expr match {
      case ScBegin(expressions, _) => evalSequence(expressions)
      case ScIf(condition, consequent, alternative, _) => evalIf(condition, consequent, alternative)
      case ScLetRec(ident, binding, body, _) => evalLetRec(ident, binding, body)
      case ScRaise(error, _) => ???
      case ScSet(variable, value, _) => ???
      case ScFunctionAp(operator, operands, _) => evalFunctionAp(operator, operands)
      case v: ScValue => evalValue(v)
      case exp: ScIdentifier => evalIdentifier(exp)
      case ScMon(contract, expression, idn) => ???
      case ScOpaque(_, refinements) => evalOpaque(refinements)
      case ScHigherOrderContract(domain, range, idn) => ???
      case ScFlatContract(expression, _) => evalFlatContract(expression)
      case lambda@ScLambda(params, _, idn) => ???
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
      result(lattice.injectOpq(Opq(refinements)))

    def evalValue(value: ScValue) = value.value match {
      case ValueInteger(i) => result(lattice.injectInteger(i))
      case ValueBoolean(b) => result(lattice.injectBoolean(b))
    }

    def evalIdentifier(identifier: ScIdentifier) =
      lookup(identifier.name).flatMap(read)

    def evalSequence(expressions: List[ScExp]): ScEvalM[PostValue] =
      sequence(expressions.map(eval)).map(_.reverse.head)

    def evalFunctionAp(operator: ScExp, operands: List[ScExp]): ScEvalM[PostValue] = for {
      evaluatedOperator <- eval(operator)
      evaluatedOperands <- sequence(operands.map(eval))
      res <- applyFn(evaluatedOperator, evaluatedOperands)
    } yield res

    def evalIf(condition: ScExp, consequent: ScExp, alternative: ScExp): ScEvalM[PostValue] =
      eval(condition).flatMap((value) => conditional(value, consequent, alternative))

    def applyFn(operator: PostValue, operands: List[PostValue]): ScEvalM[PostValue] = ???

    def conditional(condition: PostValue, consequent: ScExp, alternative: ScExp) = {
      // execute the true branch
      val t = for {
        _ <- ifFeasible(primTrue, condition)
        res <- eval(consequent)
      } yield res

      // execute the false branch
      val f = for {
        _ <- ifFeasible(primFalse, condition)
        res <- eval(alternative)
      } yield res

      // combine them non deterministically
      nondet(t, f)
    }

    def ifFeasible(op: Prim, value: PostValue): ScEvalM[()] =
      withPc(feasible(op, value)).flatMap {
        case Some(pc) => replacePc(pc)
        case None => void
      }

    private def feasible(op: Prim, value: PostValue)(pc: PC): Option[PC] =
      value._2 match {
        case _ if !lattice.isTrue(lattice.applyPrimitive(op)(value._1)) => None
        case ScNil(_) => Some(pc)
        case _ =>
          val solver = newSmtSolver(pc)
          if (solver.isSat) Some(pc.and(pc.and(ScFunctionAp(ScIdentifier(op.operation, Identity.none), List(value._2), Identity.none)))) else None
      }
  }
}
