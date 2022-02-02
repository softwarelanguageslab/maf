package maf.language.scheme

import maf.core.*
import maf.util.Trampoline
import maf.language.CScheme.CSchemeParser
import maf.language.ContractScheme.*

/**
 * Remove defines from a Scheme expression, replacing them by let bindings. For example: (define foo 1) (define (f x) x) (f foo) Will be converted to:
 * (letrec ((foo 1) (f (lambda (x) x))) (f foo)) Which is semantically equivalent with respect to the end result
 *
 * This undefiner assumes that the program is *well-formed* which means that the program is compiled by the BaseschemeCompiler (or a decentant
 * thereof) and does not contain define's in an invalid context. This can be checked by using the UndefinerTester trait.
 */
object BaseSchemeMonadicUndefiner:
    import maf.util.*
    import maf.util.Trampoline.lift
    import MonadStateT.*
    import maf.core.Monad.MonadSyntaxOps

    final lazy val Ops: StateOps[State, MS] = MonadStateT.stateInstance[State, MT]

    /**
     * The scope is a list of bindings and a reference to the outer scope
     *
     * @param bindigns
     *   a list of bindings in the current scope
     * @param uses
     *   a list of uses of variables in the current scope
     * @param outer
     *   the outer scope
     */
    case class Scope(bindings: List[(Identifier, SchemeExp)], outer: Option[Scope], topLevel: Boolean):
        def addBinding(name: Identifier, exp: SchemeExp): Scope =
          this.copy(bindings = (name -> exp) :: bindings)

        def enterScope(topLevel: Boolean): Scope =
          Scope(List(), Some(this), topLevel)

        /** Forcefully exit the current scope, even if an outer scope does not exist */
        def unsafeExitScope: Scope =
          this.outer.get

        def exitScope: Option[Scope] = this.outer

    /** The state consists of scope information */
    case class State(currentScope: Option[Scope], usages: List[Identifier], idCtr: Int):
        def enterScope(topLevel: Boolean = false): State = this.copy(
          currentScope = Some({
            currentScope match
                case None        => Scope(List(), None, topLevel)
                case Some(scope) => scope.enterScope(topLevel)
          })
        )

        def fresh(idn: Identity = Identity.none): (State, Identifier) = (this.copy(idCtr = this.idCtr + 1), Identifier(s"_${this.idCtr}", idn))

    object State:
        def empty: State = State(None, List(), 0)

    /** The trampoline monad for this computation */
    type MT = [T] =>> TrampolineT[IdentityMonad.Id, T]

    /** The state monad for this computation */
    type MS = [T] =>> MonadStateT[State, MT, T]

    /**
     * We encapsulate the state into a trampoline in order to make sure that a deeply nested program does not result in a stack overflow, but rather
     * causes a heap memory explosion.
     *
     * This is preferable in two ways: (1) it ensures that if a computation is in tail call, both the memory usage in the heap space and the memory
     * usage in the stack space remains constant. (2) if the computation is not in tail position, then the memory in the heap space will grow. However
     * it will be able to grow much longer because the maximal size of the heap space is greater than that of the stack space (usually).
     */
    type M[T] = MS[T]

    def enterScope(topLevel: Boolean = false): M[Unit] =
        import Ops.*
        for {
          currentState <- get
          _ <- put(currentState.enterScope(topLevel))
        } yield ()

    def exitScopeTopLevel: M[(List[(Identifier, SchemeExp)], Boolean)] =
        import Ops.*
        for {
          currentState <- get
          newScope = currentState.currentScope.get.exitScope
          _ <- put(currentState.copy(currentScope = newScope))
        } yield (currentState.currentScope.get.bindings.reverse, currentState.currentScope.get.topLevel)

    def fresh: M[Identifier] =
        import Ops.*
        for {
          currentState <- get
          (newState, identifier) = currentState.fresh()
          _ <- put(newState)
        } yield identifier

    /** Insert the given expression as a result in the monad */
    def mk(exp: SchemeExp): M[List[SchemeExp]] =
        import Ops.*
        for {
          topLevel <- get.map(_.currentScope.get.topLevel)
          result <-
            if topLevel then
                (for
                    id <- fresh
                    _ <- bind(id, exp)
                yield List()
            )
            else this.unit(List(exp))
        } yield result

    private def bind(name: Identifier, exp: SchemeExp): M[Unit] =
        import Ops.*
        for {
          currentState <- get
          newScope = currentState.currentScope.get.addBinding(name, exp)
          _ <- put(currentState.copy(currentScope = Some(newScope)))
        } yield ()

    private def subexpression[X](m: M[X]): M[X] =
        import Ops.*
        for {
          // fetch the current scope and its toplevel state
          currentState <- get
          topLevel = currentState.currentScope.get.topLevel
          // subexpressions are not top-level
          _ <- put(currentState.copy(currentScope = Some(currentState.currentScope.get.copy(topLevel = false))))
          // execute monadic computation
          result <- m
          // restore top-level state
          newState <- get
          _ <- put(currentState.copy(currentScope = Some(newState.currentScope.get.copy(topLevel = topLevel))))
        } yield result

    private def unit[X](v: X): M[X] =
      Ops.unit(v)

trait BaseSchemeMonadicUndefiner:
    import BaseSchemeMonadicUndefiner.*
    import maf.core.Monad.MonadSyntaxOps
    import maf.core.Monad.MonadIterableOps

    protected def letrectify(body: List[SchemeExp], bindings: List[(Identifier, SchemeExp)], topLevel: Boolean): List[SchemeExp] =
      if bindings.size == 0 then body
      else if topLevel then List(SchemeLetrec(bindings, List(SchemeVar(bindings.last._1)), Identity.none))
      else List(SchemeLetrec(bindings, body, Identity.none))

    protected def letrectify1(body: SchemeExp, bindings: List[(Identifier, SchemeExp)], topLevel: Boolean): List[SchemeExp] =
      letrectify(List(body), bindings, topLevel)

    private def usingNewScope(f: M[List[SchemeExp]]): M[(List[SchemeExp], List[(Identifier, SchemeExp)], Boolean)] =
      for {
        _ <- enterScope()
        undefinedExpr <- f
        result <- exitScopeTopLevel
        (bindings, isTopLevel) = result
      } yield (undefinedExpr, bindings, isTopLevel)

    /** Same as undefine1 but asserts that only one expression is returned in the list */
    private def undefineSingle(exp: SchemeExp): M[SchemeExp] = for
        e <- subexpression {
          for
              es <- undefine1(exp)
              _ = { assert(es.size == 1) }
              e = es.head
          yield e
        }
    yield e

    def undefineBinding(idf: Identifier, exp: SchemeExp): M[(Identifier, SchemeExp)] =
      undefineSingle(exp).map((idf, _))

    def undefine(exps: List[SchemeExp]): M[List[SchemeExp]] =
      Monad.sequence(exps.map(undefine1)).map(_.flatten)

    def undefine1(exps: SchemeExp): M[List[SchemeExp]] = exps match
        case SchemeLambda(name, args, body, ann, pos) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) flatMap (b => mk(SchemeLambda(name, args, b, ann, pos)))

        case SchemeVarArgLambda(name, args, vararg, body, ann, pos) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) flatMap (b => mk(SchemeVarArgLambda(name, args, vararg, b, ann, pos)))

        case SchemeIf(cond, cons, alt, idn) =>
          for
              undefineCond <- undefineSingle(cond)
              undefineCons <- undefineSingle(cons)
              undefineAlt <- undefineSingle(alt)
              result <- mk(SchemeIf(undefineCond, undefineCons, undefineAlt, idn))
          yield result

        case SchemeFuncall(f, args, pos) =>
          for
              undefinedF <- undefineSingle(f)
              undefinedArgs <- args.mapM(undefineSingle)
              result <- mk(SchemeFuncall(undefinedF, undefinedArgs, pos))
          yield result

        case SchemeLet(bindings, body, idn) =>
          for
              undefinedBds <- bindings.mapM(undefineBinding.tupled)
              undefinedBdy <- usingNewScope { undefine(body) } map (letrectify.tupled)
              result <- mk(SchemeLet(undefinedBds, undefinedBdy, idn))
          yield result

        case SchemeLetStar(bindings, body, idn) =>
          for
              undefinedBds <- bindings.mapM(undefineBinding.tupled)
              undefinedBdy <- usingNewScope { undefine(body) } map (letrectify.tupled)
              result <- mk(SchemeLetStar(undefinedBds, undefinedBdy, idn))
          yield result

        case SchemeLetrec(bindings, body, idn) =>
          for
              undefinedBds <- bindings.mapM(undefineBinding.tupled)
              undefinedBdy <- usingNewScope { undefine(body) } map (letrectify.tupled)
              result <- mk(SchemeLetrec(undefinedBds, undefinedBdy, idn))
          yield result

        case SchemeSet(variable, value, idn) =>
          undefineSingle(value) flatMap (b => mk(SchemeSet(variable, b, idn)))

        case SchemeSetLex(variable, lexAddr, value, idn) =>
          undefineSingle(value) flatMap (v => mk(SchemeSetLex(variable, lexAddr, v, idn)))

        // begin's do not introduce an additional scope.
        case SchemeBegin(exps, idn) =>
          for
              undefineExps <- undefine(exps)
              result <- if undefineExps.isEmpty then unit(List()) else mk(SchemeBegin(undefineExps, idn))
          yield result

        case SchemeDefineVariable(name, value, idn) =>
          for
              undefinedExp <- undefineSingle(value)
              _ <- bind(name, undefinedExp)
          yield List()

        // variables and valus do not contain any subexpressions
        case _: SchemeVar | _: SchemeVarLex | _: SchemeValue => mk(exps)

        case SchemeAssert(exp, idn) =>
          undefineSingle(exp) flatMap (exp => mk(SchemeAssert(exp, idn)))

        // Fork introduces a new body
        case CSchemeFork(body, idn) =>
          undefineSingle(body) flatMap (body => mk(CSchemeFork(body, idn)))

        case CSchemeJoin(texp, idn) =>
          undefineSingle(texp) flatMap (texp => mk(CSchemeJoin(texp, idn)))

        case SchemeCodeChange(old, nw, idn) =>
          for
              undefineOld <- undefineSingle(old)
              undefineNew <- undefineSingle(nw)
              result <- mk(SchemeCodeChange(undefineOld, undefineNew, idn))
          yield result

        // define's are not allowed in the expressions of the domain and rangeMaker,
        // unless they themselves introduce bodies in which case it has already
        // been handled inside of the recursion
        case ContractSchemeDepContract(domains, rangeMaker, idn) =>
          for
              undefineDomains <- domains.mapM(undefineSingle)
              undefineRangeMaker <- undefineSingle(rangeMaker)
              result <- mk(ContractSchemeDepContract(domains, rangeMaker, idn))
          yield result

        case ContractSchemeMon(contract, expression, idn) =>
          for
              undefineContract <- undefineSingle(contract)
              undefineExpression <- undefineSingle(expression)
              result <- mk(ContractSchemeMon(undefineContract, undefineExpression, idn))
          yield result

        case ContractSchemeCheck(contract, valueExpression, idn) =>
          for
              undefineContract <- undefineSingle(contract)
              undefineValue <- undefineSingle(valueExpression)
              result <- mk(ContractSchemeCheck(undefineContract, undefineValue, idn))
          yield result

        case ContractSchemeFlatContract(expression, idn) =>
          undefineSingle(expression) flatMap (e => mk(ContractSchemeFlatContract(e, idn)))
        case MatchExpr(value, clauses, idn) =>
          for
              undefinedValue <- undefineSingle(value)
              undefinedClauses <- Monad.sequence(clauses.map(cl => subexpression { undefine(cl.expr).map(MatchExprClause(cl.pat, _, cl.whenExpr)) }))
              result <- mk(MatchExpr(undefinedValue, undefinedClauses, idn))
          yield result

        case ContractSchemeDefineContract(name, params, contract, expression, idn) =>
          throw new Exception("should be translated in the ContractSchemeCompiler")
        case ContractSchemeProvide(outs, idn) =>
          mk(ContractSchemeProvide(outs, idn))
        case _: MakeStruct => mk(exps)

object SchemeMonadicUndefiner extends BaseSchemeMonadicUndefiner, UndefinerTester:
    import BaseSchemeMonadicUndefiner.*
    import maf.core.Monad.MonadSyntaxOps

    def undefineExps(exps: List[SchemeExp], topLevel: Boolean = true): List[SchemeExp] =
        val result = check(exps, true)
        if result.isError then
            throw new Exception(s"Malformed program, define in invalid context. First occurence of error at ${result.show} in $exps")
        val computation = (for
            _ <- enterScope(topLevel)
            undefinedExps <- undefine(exps)
            result <- exitScopeTopLevel
            (bindings, _) = result
        yield letrectify(undefinedExps, bindings, topLevel))
        Trampoline.run[List[SchemeExp]](computation.runValue(State.empty))

def test(): Unit =
    import maf.util.Reader
    import maf.language.scheme.*
    val contents = Reader.loadFile("/tmp/test.scm")
    val parsed = List(ContractSchemeParser.compile(contents))
    println(parsed.map(_.prettyString(0)).mkString("\n"))
    //val output = CSchemeParser.parseProgram(contents)
    val output = SchemeBegin(SchemeMonadicUndefiner.undefineExps(parsed, true), Identity.none)
    println(contents)
    println("================== translated ==================")
    println(output.prettyString(0))
