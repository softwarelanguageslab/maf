package maf.modular.scheme.modf

import maf.core.Position._
import maf.core._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.language.sexp
import maf.modular._
import maf.modular.components.ContextSensitiveComponents
import maf.modular.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.language.CScheme.TID
import maf.util._
import maf.core.IdentityMonad.given
import maf.core.Monad.MonadIterableOps
import maf.core.Monad.MonadSyntaxOps
import maf.save.SaveModF
import maf.save.Savable

trait BaseEvalM[M[_]] extends Monad[M] with MonadError[M, Error] with MonadJoin[M]

/** Base definitions for a Scheme MODF analysis (monadic style). */
// TODO: Most of this can be factored out to SchemeSemantics
trait BaseSchemeModFSemanticsM
    extends ModAnalysis[SchemeExp]
    with GlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SchemeDomain
    with ContextSensitiveComponents[SchemeExp]:

    /** Functions of these base definitions can be executed in the context of the given monad */
    type M[_]
    implicit lazy val baseEvalM: BaseEvalM[M]

    // the environment in which the ModF analysis is executed
    type Env = Environment[Addr]
    def baseEnv: Env

    // In ModF, components are function calls in some context.
    // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.
    def view(cmp: Component): SchemeModFComponent

    /** Get the source path for the given SchemeExpr if available */
    def getName(program: SchemeExp): String =
        import maf.core.Position
        program.idn.pos.tag match
            case Position.PTagWithSource(_, source) => source
            case Position.SourcePathTag(source)     => source
            case _                                  => "unknown"

    /** A warning printer */
    def warn(msg: String): Unit =
        MAFLogger.log(MAFLogger.LogLevel.AnalysisError, s"[${getName(program)}] warn $msg")

    // Represent `allocCtx` as a value, which can be passed
    // to other functions
    protected trait ContextBuilder:
        def allocM(
            clo: (SchemeLambdaExp, Environment[Address]),
            args: List[Value],
            call: Position,
            caller: Component
          ): M[ComponentContext]

        /** Hook that gets executed after the context has been allocated, but before the function is (semantically) being called */
        def beforeCall(cmp: Component, prs: List[Identifier], clo: (SchemeLambdaExp, Environment[Address])): M[Unit] = Monad[M].unit(())

    protected object DefaultContextBuilder extends ContextBuilder:
        def allocM(
            clo: (SchemeLambdaExp, Environment[Address]),
            args: List[Value],
            call: Position,
            caller: Component
          ): M[ComponentContext] = Monad[M].unit(allocCtx(clo, args, call, caller))

    lazy val mainBody = program
    def expr(cmp: Component): SchemeExp = body(cmp)
    def body(cmp: Component): SchemeExp = body(view(cmp))
    def body(cmp: SchemeModFComponent): SchemeExp = cmp match
        case Main       => mainBody
        case c: Call[_] => SchemeBody(c.lambda.body)

    type ComponentContent = Option[(SchemeLambdaExp, Environment[Address])]
    def content(cmp: Component) = view(cmp) match
        case Main       => None
        case c: Call[_] => Some(c.clo)
    def context(cmp: Component): Option[ComponentContext] = view(cmp) match
        case Main                                 => None
        case c: Call[ComponentContext] @unchecked => Some(c.ctx)

    /** Creates a new component, given a closure, context and an optional name. */
    def newComponent(call: Call[ComponentContext]): Component

    /** Creates a new context given a closure, a list of argument values and the position of the call site. */
    def allocCtx(
        clo: (SchemeLambdaExp, Environment[Address]),
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext

    /** Allocating addresses */
    type AllocationContext
    def allocVar(id: Identifier, cmp: Component): VarAddr[AllocationContext]
    def allocPtr(exp: SchemeExp, cmp: Component): PtrAddr[AllocationContext]

    /* convience accessors for argument values */
    def argValues(cmp: Component): Map[String, Value] = view(cmp) match
        case Main => Map.empty
        case Call((lam, _), _) =>
            val argAddrs = lam.args.map(id => (id.name, allocVar(id, cmp))).toMap
            val allArgAddrs = lam.varArgId match
                case None         => argAddrs
                case Some(varArg) => argAddrs + (varArg.name -> allocVar(varArg, cmp))
            allArgAddrs.map(bnd => (bnd._1, store.getOrElse(bnd._2, lattice.bottom)))

    /* convience accessor for getting the argument values in the order of their definition */
    def argValuesList(cmp: Component): List[(Addr, Value)] = view(cmp) match
        case Main => List()
        case Call((lam, _), _) =>
            val argAddrs = lam.args.map(id => allocVar(id, cmp))
            val allArgAddrs = lam.varArgId match
                case None         => argAddrs
                case Some(varArg) => argAddrs ++ List(allocVar(varArg, cmp))

            allArgAddrs.map(addr => (addr, store.getOrElse(addr, lattice.bottom)))

    protected def newComponentM(call: Call[ComponentContext]): M[Component] =
        baseEvalM.unit(newComponent(call))

    //XXXXXXXXXXXXXXXXXXXXXXXXXX//
    // INTRA-COMPONENT ANALYSIS //
    //XXXXXXXXXXXXXXXXXXXXXXXXXX//

    // Extensions to the intraAnalysis.
    override def intraAnalysis(cmp: Component): SchemeModFSemanticsIntra
    trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra { modf =>
        // components
        protected def fnBody: SchemeExp = body(view(component))
        protected def fnArgs(cmp: Component): Array[Address] = (view(cmp) match
            case Main => List()
            case c: Call[_] =>
                c.lambda.args.map { id =>
                    allocVar(id, cmp)
                } ++ (c.lambda.varArgId match
                    case None         => List()
                    case Some(varArg) => List(allocVar(varArg, cmp))
                )
        ).toArray
        protected def fnArgs: Array[Address] = fnArgs(component)

        protected def fnEnv: Env = view(component) match
            case Main => baseEnv
            case c: Call[_] =>
                val extEnv = c.env.extend(c.lambda.args.map { id =>
                    (id.name, allocVar(id, component))
                })
                c.lambda.varArgId match
                    case None         => extEnv
                    case Some(varArg) => extEnv.extend(varArg.name, allocVar(varArg, component))
        // variable lookup: use the global store
        protected def lookup(id: Identifier, env: Env): M[Value] = env.lookup(id.name) match
            case None       => baseEvalM.fail(UndefinedVariableError(id))
            case Some(addr) => read(addr)

        protected def read(adr: Addr): M[Value] =
            baseEvalM.unit(readAddr(adr))
        protected def write(adr: Addr, v: Value): M[Unit] =
            baseEvalM.unit(writeAddr(adr, v))

        protected def assign(
            id: Identifier,
            env: Env,
            vlu: Value
          ): M[Unit] = env.lookup(id.name) match
            case None => baseEvalM.fail(UndefinedVariableError(id))
            case Some(addr) =>
                write(addr, vlu)

        protected def assign(bds: List[(Identifier, Value)], env: Env): M[Unit] =
            import maf.core.Monad.MonadSyntaxOps
            Monad.sequence(bds.map { case (id, vlu) => assign(id, env, vlu) }) >>> baseEvalM.unit(())

        protected def bind(
            id: Identifier,
            env: Env,
            vlu: Value
          ): M[Env] =
            val addr = allocVar(id, component)
            val env2 = env.extend(id.name, addr)
            write(addr, vlu) >>> baseEvalM.unit(env2)

        protected def bind(bds: List[(Identifier, Value)], env: Env): M[Env] =
            bds.foldLeftM(env)((env2, bnd) => bind(bnd._1, env2, bnd._2))

        protected def applyFun(
            fexp: SchemeFuncall,
            fval: Value,
            args: List[(SchemeExp, Value)],
            cll: Position,
            ctx: ContextBuilder = DefaultContextBuilder,
          ): M[Value] =
            import maf.core.Monad.MonadSyntaxOps
            val fromClosures = applyClosuresM(fval, args, cll, ctx)
            val fromPrimitives = applyPrimitives(fexp, fval, args)
            val _ = applyContinuations(fval, args)
            baseEvalM.mjoin(List(fromClosures, fromPrimitives))

        protected def applyContinuations(k: Value, args: List[(SchemeExp, Value)]) =
            args match
                case (_, vlu) :: Nil =>
                    val cnts = lattice.getContinuations(k)
                    cnts.foreach(cnt => writeResult(vlu, cnt.asInstanceOf[Component])) // TODO: type safety!
                case _ => () // continuations are only called with a single argument
        // => ignore continuation calls with more than 1 argument

        /**
         * Hook for code that needs to be executed after the call
         *
         * @param targetCmp
         *   the called component
         * @return
         *   nothing
         */
        protected def afterCall(vlu: Value, cmp: Component, cll: Position): M[Value] = Monad[M].unit(vlu)

        protected def applyClosuresM(
            fun: Value,
            args: List[(SchemeExp, Value)],
            cll: Position,
            ctx: ContextBuilder = DefaultContextBuilder,
          ): M[Value] =
            val arity = args.length
            val closures = lattice.getClosures(fun)
            MonadJoin[M].mfoldMap(closures)((clo) =>
                (clo match {
                    case (SchemeLambda(_, prs, _, _, _), _) =>
                        if prs.length == arity then
                            val argVals = args.map(_._2)
                            for
                                context <- ctx.allocM(clo, argVals, cll, component)
                                targetCall = Call(clo, context)
                                targetCmp <- newComponentM(targetCall)
                                _ = bindArgs(targetCmp, prs, argVals)
                                _ <- ctx.beforeCall(targetCmp, prs, clo)
                                result = call(targetCmp)
                                updatedResult <- afterCall(result, targetCmp, cll)
                            yield updatedResult
                        else baseEvalM.fail(ArityError(cll, prs.length, arity))
                    case (SchemeVarArgLambda(_, prs, vararg, _, _, _), _) =>
                        if prs.length <= arity then
                            val (fixedArgs, varArgs) = args.splitAt(prs.length)
                            val fixedArgVals = fixedArgs.map(_._2)

                            for
                                varArgVal <- allocateList(varArgs)
                                context <- ctx.allocM(clo, fixedArgVals :+ varArgVal, cll, component)
                                targetCall = Call(clo, context)
                                targetCmp <- newComponentM(targetCall)
                                _ = bindArgs(targetCmp, prs, fixedArgVals)
                                _ = bindArg(targetCmp, vararg, varArgVal)
                                _ <- ctx.beforeCall(targetCmp, prs, clo)
                                result = call(targetCmp)
                                updatedResult <- afterCall(result, targetCmp, cll)
                            yield updatedResult
                        else baseEvalM.fail(VarArityError(cll, prs.length, arity))
                    case _ => Monad[M].unit(lattice.bottom)
                })
            )
        protected def allocateList(elms: List[(SchemeExp, Value)]): M[Value] = elms match
            case Nil => baseEvalM.unit(lattice.nil)
            case (exp, vlu) :: rest =>
                allocateList(rest).flatMap(v => allocateCons(exp)(vlu, v))

        protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): M[Value] =
            allocateVal(pairExp)(lattice.cons(car, cdr))
        protected def allocateString(stringExp: SchemeExp)(str: String): M[Value] =
            allocateVal(stringExp)(lattice.string(str))
        protected def allocateVal(exp: SchemeExp)(v: Value): M[Value] =
            val addr = allocPtr(exp, component)
            write(addr, v) >>> baseEvalM.unit(lattice.pointer(addr))

        protected def append(appendExp: SchemeExp)(l1: (SchemeExp, Value), l2: (SchemeExp, Value)): Value =
            //TODO [difficult]: implement append
            throw new Exception("NYI -- append")
        protected def bindArg(
            component: Component,
            par: Identifier,
            arg: Value
          ): Unit =
            writeAddr(allocVar(par, component), arg)

        protected def bindArgs(
            component: Component,
            pars: List[Identifier],
            args: List[Value]
          ): Unit =
            pars.zip(args).foreach { case (par, arg) => bindArg(component, par, arg) }

        protected def currentThread: TID =
            throw new Exception("Concurrency not available in ModF")
        given interpreterBridge: SchemeInterpreterBridge[Value, Addr] with
            def pointer(exp: SchemeExp): Addr = allocPtr(exp, component)
            def readSto(adr: Addr): Value = readAddr(adr)
            def writeSto(adr: Addr, vlu: Value) = writeAddr(adr, vlu)
            def callcc(
                clo: (SchemeLambdaExp, Environment[Address]),
                fpos: Position
              ): Value = modf.callcc(clo, fpos)
            def currentThread = modf.currentThread
        protected def applyPrimitives(
            fexp: SchemeFuncall,
            fval: Value,
            args: List[(SchemeExp, Value)]
          ): M[Value] =
            MonadJoin[M].mfoldMap(lattice.getPrimitives(fval))(prm =>
                (primitives(prm).callMF(fexp, args.map(_._2)) match {
                    case MayFailSuccess(vlu) => Monad[M].unit(vlu)
                    case MayFailBoth(vlu, e) => MonadJoin[M].mjoin(Monad[M].unit(vlu), MonadError[M, Error].fail(PrimitiveError(e)))
                    case MayFailError(e)     => MonadError[M, Error].fail(PrimitiveError(e))
                })
            )
        // evaluation helpers
        protected def evalLiteralValue(literal: sexp.Value, exp: SchemeExp): M[Value] = literal match
            case sexp.Value.Integer(n)   => baseEvalM.unit(lattice.number(n))
            case sexp.Value.Real(r)      => baseEvalM.unit(lattice.real(r))
            case sexp.Value.Boolean(b)   => baseEvalM.unit(lattice.bool(b))
            case sexp.Value.String(s)    => allocateString(exp)(s)
            case sexp.Value.Character(c) => baseEvalM.unit(lattice.char(c))
            case sexp.Value.Symbol(s)    => baseEvalM.unit(lattice.symbol(s))
            case sexp.Value.Nil          => baseEvalM.unit(lattice.nil)
        // The current component serves as the lexical environment of the closure.
        protected def newClosure(
            lambda: SchemeLambdaExp,
            env: Env
          ): Value =
            lattice.closure((lambda, env.restrictTo(lambda.fv)))

        protected def callcc(
            closure: (SchemeLambdaExp, Environment[Address]),
            fpos: Position
          ): Value =
            val ctx = allocCtx(closure, Nil, fpos, component)
            val cll = Call(closure, ctx)
            val cmp = newComponent(cll)
            val cnt = lattice.cont(cmp)
            val par = closure._1.args.head
            bindArg(cmp, par, cnt)
            call(cmp)

        // other helpers
        protected def conditional[M: Monoid](
            prd: Value,
            csq: => M,
            alt: => M
          ): M =
            val csqVal = if lattice.isTrue(prd) then csq else Monoid[M].zero
            val altVal = if lattice.isFalse(prd) then alt else Monoid[M].zero
            Monoid[M].append(csqVal, altVal)
    }

/* Legacy support for non-monadic base semantics */
//trait BaseSchemeModFSemantics extends BaseSchemeModFSemanticsM:
//    export maf.core.IdentityMonad.given
//    import maf.core.IdentityMonad.Id
//
//    type M[X] = Id[X]
//    implicit final val baseEvalM: Monad[M] = idMonad
trait BaseSchemeModFSemantics extends BaseSchemeModFSemanticsM
trait BaseSchemeModFSemanticsIdentity extends BaseSchemeModFSemantics:
    export maf.core.IdentityMonad.given
    import maf.core.IdentityMonad.Id

    sealed trait IdFailure[+X]:
        def force: X = this match
            case Success(v) => v
            case Failure    => throw new Exception("cannot force failure")

    case class Success[X](v: X) extends IdFailure[X]
    case object Failure extends IdFailure[Nothing]

    type M[X] = IdFailure[X]
    implicit lazy val baseEvalM = new BaseEvalM[M]:
        def unit[T](v: T) = Success(v)
        def flatMap[A, B](m: M[A])(f: A => M[B]): M[B] =
            m match
                case Success(v) => f(v)
                case Failure    => Failure
        def map[A, B](m: M[A])(f: A => B): M[B] =
            flatMap(m)((a) => unit(f(a)))
        def fail[X](e: Error): M[X] =
            warn(s"encountered an error $e")
            Failure
        def mbottom[X]: M[X] = Failure
        def mjoin[X: Lattice](x: M[X], y: M[X]): M[X] =
            (x, y) match
                case (Success(v), Failure)      => Success(v)
                case (Failure, Success(v))      => Success(v)
                case (Success(v1), Success(v2)) => Success(Lattice[X].join(v1, v2))
                case _                          => Failure

    /** Implicitly tries to convert the given computation to a lattice value. If the computation results in  an error bottom is Return */
    implicit def mtry(m: M[Value]): Value =
        m match
            case Success(v) => v
            case _          => lattice.bottom

trait SchemeModFSemanticsM extends SchemeSetup with BaseSchemeModFSemanticsM with StandardSchemeModFAllocator:
    def baseEnv = initialEnv

trait SchemeModFSemantics extends BaseSchemeModFSemanticsIdentity

// for convenience, since most Scheme analyses don't need this much parameterization
abstract class SimpleSchemeModFAnalysis(prg: SchemeExp, override val name: Option[String] = None)
    extends ModAnalysis[SchemeExp](prg)
    with StandardSchemeModFComponents
    with SchemeModFSemanticsM
    with BigStepModFSemantics
    with SaveModF:
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
