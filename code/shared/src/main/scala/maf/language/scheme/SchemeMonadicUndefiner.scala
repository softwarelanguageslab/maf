package maf.language.scheme

import maf.core.*
import maf.util.Trampoline

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
    case class Scope(bindings: List[(Identifier, SchemeExp)], outer: Option[Scope]):
        def addBinding(name: Identifier, exp: SchemeExp): Scope =
          this.copy(bindings = (name -> exp) :: bindings)

        def enterScope: Scope =
          Scope(List(), Some(this))

        /** Forcefully exit the current scope, even if an outer scope does not exist */
        def unsafeExitScope: Scope =
          this.outer.get

        def exitScope: Option[Scope] = this.outer

    /** The state consists of scope information */
    case class State(currentScope: Option[Scope], usages: List[Identifier]):
        def enterScope: State = this.copy(currentScope = Some({
          currentScope match
              case None        => Scope(List(), None)
              case Some(scope) => scope.enterScope
        }))

    object State:
        def empty: State = State(None, List())

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

    def enterScope: M[Unit] =
        import Ops.*
        for {
          currentState <- get
          _ <- put(currentState.enterScope)
        } yield ()

    def exitScope: M[List[(Identifier, SchemeExp)]] =
        import Ops.*
        for {
          currentState <- get
          newScope = currentState.currentScope.get.exitScope
          _ <- put(currentState.copy(currentScope = newScope))
        } yield currentState.currentScope.get.bindings.reverse

    private def bind(name: Identifier, exp: SchemeExp): M[Unit] =
        import Ops.*
        for {
          currentState <- get
          newScope = currentState.currentScope.get.addBinding(name, exp)
          _ <- put(currentState.copy(currentScope = Some(newScope)))
        } yield ()

    private def unit[X](v: X): M[X] =
      Ops.unit(v)

trait BaseSchemeMonadicUndefiner:
    import BaseSchemeMonadicUndefiner.*
    import maf.core.Monad.MonadSyntaxOps
    import maf.core.Monad.MonadIterableOps

    protected def letrectify(body: List[SchemeExp], bindings: List[(Identifier, SchemeExp)]): List[SchemeExp] =
      List(SchemeLetrec(bindings, body, Identity.none))

    protected def letrectify1(body: SchemeExp, bindings: List[(Identifier, SchemeExp)]): List[SchemeExp] = letrectify(List(body), bindings)

    private def usingNewScope(f: M[List[SchemeExp]]): M[(List[SchemeExp], List[(Identifier, SchemeExp)])] =
      for {
        _ <- enterScope
        undefinedExpr <- f
        bindings <- exitScope
      } yield (undefinedExpr, bindings)

    /** Same as undefine1 but asserts that only one expression is returned in the list */
    private def undefineSingle(exp: SchemeExp): M[SchemeExp] = for
        es <- undefine1(exp)
        _ = { assert(es.size == 1) }
        e = es.head
    yield e

    def undefine(exps: List[SchemeExp]): M[List[SchemeExp]] =
      Monad.sequence(exps.map(undefine1)).map(_.flatten)

    def undefine1(exps: SchemeExp): M[List[SchemeExp]] = exps match
        case SchemeLambda(name, args, body, ann, pos) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) map (b => List(SchemeLambda(name, args, b, ann, pos)))

        case SchemeVarArgLambda(name, args, vararg, body, ann, pos) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) map (b => List(SchemeVarArgLambda(name, args, vararg, b, ann, pos)))

        case SchemeIf(cond, cons, alt, idn) =>
          for
              undefineCond <- undefineSingle(cond)
              undefineCons <- undefineSingle(cons)
              undefineAlt <- undefineSingle(alt)
          yield List(SchemeIf(undefineCond, undefineCons, undefineAlt, idn))

        case SchemeFuncall(f, args, pos) =>
          for
              undefinedF <- undefineSingle(f)
              undefinedArgs <- args.mapM(undefineSingle)
          yield List(SchemeFuncall(undefinedF, undefinedArgs, pos))

        case SchemeLet(bindings, body, idn) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) map (b => List(SchemeLet(bindings, b, idn)))

        case SchemeLetStar(bindings, body, idn) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) map (b => List(SchemeLetStar(bindings, b, idn)))

        case SchemeLetrec(bindings, body, idn) =>
          usingNewScope { undefine(body) } map (letrectify.tupled) map (b => List(SchemeLetrec(bindings, b, idn)))

        case SchemeSet(variable, value, idn) =>
          for
              undefinedValue <- undefine1(value)
              _ = { assert(undefinedValue.size == 1) } // invariant
              undefinedValue1 = undefinedValue.head
          yield List(SchemeSet(variable, undefinedValue1, idn))

        // same reasoning as with SchemeSet
        case SchemeSetLex(variable, lexAddr, value, idn) =>
          unit(List(SchemeSetLex(variable, lexAddr, value, idn)))

        // begin's do not introduce an additional scope.
        case SchemeBegin(exps, idn) =>
          for undefineExps <- undefine(exps)
          yield if undefineExps.isEmpty then List() else List(SchemeBegin(undefineExps, idn))

        case SchemeDefineVariable(name, value, idn) =>
          for
              undefinedExp <- undefineSingle(value)
              _ <- bind(name, undefinedExp)
          yield List()

        // variables and valus do not contain any subexpressions
        case _: SchemeVar | _: SchemeVarLex | _: SchemeValue => unit(List(exps))

        case SchemeAssert(exp, idn) =>
          undefineSingle(exp) map (exp => List(SchemeAssert(exp, idn)))

        // Fork introduces a new body
        case CSchemeFork(body, idn) =>
          undefineSingle(body) map (body => List(CSchemeFork(body, idn)))

        case CSchemeJoin(texp, idn) =>
          undefineSingle(texp) map (texp => List(CSchemeJoin(texp, idn)))

        case SchemeCodeChange(old, nw, idn) =>
          for
              undefineOld <- undefineSingle(old)
              undefineNew <- undefineSingle(nw)
          yield List(SchemeCodeChange(undefineOld, undefineNew, idn))

        // define's are not allowed in the expressions of the domain and rangeMaker,
        // unless they themselves introduce bodies in which case it has already
        // been handled inside of the recursion
        case ContractSchemeDepContract(domains, rangeMaker, idn) =>
          for
              undefineDomains <- domains.mapM(undefineSingle)
              undefineRangeMaker <- undefineSingle(rangeMaker)
          yield List(ContractSchemeDepContract(domains, rangeMaker, idn))

        case ContractSchemeFlatContract(expression, idn) =>
          for undefineExpression <- undefineSingle(expression)
          yield List(ContractSchemeFlatContract(undefineExpression, idn))

        case ContractSchemeDefineContract(name, params, contract, expression, idn) =>
          throw new Exception("should be translated in the ContractSchemeCompiler")
        case ContractSchemeProvide(outs, idn) =>
          unit(List(ContractSchemeProvide(outs, idn)))
        case _: MakeStruct => unit(List(exps))

object SchemeMonadicUndefiner extends BaseSchemeMonadicUndefiner:
    import BaseSchemeMonadicUndefiner.*
    import maf.core.Monad.MonadSyntaxOps

    def undefineExps(exps: List[SchemeExp]): List[SchemeExp] =
        val computation = (for
            _ <- enterScope
            undefinedExps <- undefine(exps)
            bindings <- exitScope
        yield letrectify(undefinedExps, bindings))
        Trampoline.run[List[SchemeExp]](computation.runValue(State.empty))

def test(): Unit =
    import maf.util.Reader
    import maf.language.scheme.*
    val contents = Reader.loadFile("/tmp/test.scm")
    val parsed = SchemeParser.parse(contents)
    val output = SchemeBegin(SchemeMonadicUndefiner.undefineExps(parsed), Identity.none)
    println(contents)
    println("================== translated ==================")
    println(output.prettyString(0))
