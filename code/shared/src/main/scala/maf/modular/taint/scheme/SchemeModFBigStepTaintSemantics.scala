package maf.modular.taint.scheme

import maf.core.IdentityMonad
import maf.language.change.CodeVersion.*
import maf.core.*
import maf.core.Position.Position
import maf.language.scheme.*
import maf.language.sexp
import maf.modular.taint.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.util.benchmarks.Timeout
import maf.modular.*
import maf.modular.scheme.modf.SchemeModFComponent.Call

/** Implements big-step semantics for a Scheme DF analysis. * */
trait SchemeModFBigStepTaintSemantics
    extends ModAnalysis[SchemeExp]
    with BigStepModFSemanticsT
    with IncrementalSchemeDomain
    with GlobalStoreTaint[SchemeExp]:

    type EvalM[X] = TaintEvalM[X]
    implicit val evalM: MonadTaintEvalM = new MonadTaintEvalM {}

    case class TaintEvalM[+X](run: (Set[Address], Environment[Address]) => Option[X]):
        def flatMap[Y](f: X => TaintEvalM[Y]): TaintEvalM[Y] =
            TaintEvalM((implicitFlows, env) => run(implicitFlows, env).flatMap(res => f(res).run(implicitFlows, env)))
        def map[Y](f: X => Y): TaintEvalM[Y] = TaintEvalM((imF, env) => run(imF, env).map(f))

        def fail[X](e: Error): EvalM[X] = throw new Exception(e.toString)

    trait MonadTaintEvalM extends TEvalM[TaintEvalM]:
        def map[X, Y](m: TaintEvalM[X])(f: X => Y): TaintEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: TaintEvalM[X])(f: X => TaintEvalM[Y]): TaintEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): TaintEvalM[X] = TaintEvalM((_, _) => Some(x))
        def mzero[X]: TaintEvalM[X] = TaintEvalM((_, _) => None)
        //def guard(cnd: Boolean): TaintEvalM[Unit] = if cnd then TaintEvalM(_ => Some(())) else mzero
        // TODO: Scala probably already has something for this?
        implicit class MonadicOps[X](xs: Iterable[X]):
            def foldLeftM[Y](y: Y)(f: (Y, X) => TaintEvalM[Y]): TaintEvalM[Y] = xs match
                case Nil     => unit(y)
                case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
            def mapM[Y](f: X => TaintEvalM[Y]): TaintEvalM[List[Y]] = xs match
                case Nil => unit(Nil)
                case x :: xs =>
                    for
                        fx <- f(x)
                        rest <- xs.mapM(f)
                    yield fx :: rest
            def mapM_(f: X => TaintEvalM[Unit]): TaintEvalM[Unit] = xs match
                case Nil     => unit(())
                case x :: xs => f(x).flatMap(_ => xs.mapM_(f))
        def getEnv: TaintEvalM[Environment[Address]] = TaintEvalM((_, env) => Some(env))
        // TODO: withExtendedEnv would make more sense
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => TaintEvalM[X]): TaintEvalM[X] =
            TaintEvalM((imF, env) => ev.run(imF, f(env)))
        def getImplicitTaint: TaintEvalM[Set[Address]] = TaintEvalM((t, _) => Some(t))
        def withImplicitTaint[X](ts: Set[Addr])(ev: => TaintEvalM[X]): TaintEvalM[X] = TaintEvalM((imF, env) => ev.run(ts, env))
        def addImplicitTaint[X](ts: Set[Addr])(ev: => TaintEvalM[X]): TaintEvalM[X] = TaintEvalM((imF, env) => ev.run(imF ++ ts, env))
        def merge[X: Lattice](x: TaintEvalM[X], y: TaintEvalM[X]): TaintEvalM[X] = TaintEvalM { (imF, env) =>
            (x.run(imF, env), y.run(imF, env)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1, res2))
        }

        def fail[X](e: Error): TaintEvalM[X] = mzero // throw new Exception(e.toString)

    override def warn(msg: String): Unit = ()

    var badFlows: Set[(Addr, Addr)] = Set()

    def traceDataFlow(addr: Set[Addr]): Unit =
        var work: Set[Addr] = addr
        var visited: Set[Addr] = Set()
        while work.nonEmpty do
            val first = work.head
            work = work - first
            if !visited.contains(first) then
                visited = visited + first
                val prev: Set[Addr] = dataFlowR(first)
                val src = prev.filter(_.isInstanceOf[SrcAddr[_]])
                if src.nonEmpty then
                    // A harmful flow was found.
                    src.foreach(s => badFlows = badFlows + ((s, addr.head))) // Initially, only contains the address of the variable read by sink.
                prev.foreach { p =>
                    if !p.isInstanceOf[SanAddr[_]] then work += p
                }

    def taintResult(): String =
        traceDataFlow(store.keySet.filter(_.isInstanceOf[SnkAddr[_]]))
        if badFlows.isEmpty then "No bad flows found."
        else "Some source values may reach sinks:" + badFlows.map(f => s"${f._1} => ${f._2}").mkString("\n", "\n", "\n")

    trait SchemeModFBigStepTaintIntra extends IntraAnalysis with BigStepModFIntraT with GlobalStoreTaintIntra:
        import evalM._

        // analysis entry point
        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(Set[Addr](), fnEnv).foreach(res => writeResult(res))

        override def eval(exp: SchemeExp): EvalM[Value] = exp match
            case SchemeSource(name, _) =>
                // Like evalVariable, but adds a source tag.
                //val a = SrcAddr(name)
                //evalVariable(name).map(v => { write(a, v); read(a) }) // Store threading is needed for sanitizers to be able to stop the trace. TODO is it?
                evalVariable(name).map(v => lattice.addAddress(v, SrcAddr(name, context(component))))
            case SchemeSanitizer(name, _) => // TODO why can't we just remove the flow information?
                val a = SanAddr(name, context(component))
                for
                    v <- evalVariable(name)
                    _ <- write(a, v)
                    rv <- read(a)
                yield rv
            case SchemeSink(name, _) =>
                // Thread sinks through the store so they can be found again afterwards.
                // Tracing afterwards is needed since components may not be reanalysed when the arguments remain the same (but the explicit taints change).
                val a = SnkAddr(name, context(component))
                for
                    v <- evalVariable(name)
                    _ <- write(a, v) // Takes implicit flows into account!
                    rv <- read(a)
                yield rv
            case _ => super.eval(exp)

        // Add the implicit taint to the value written.
        override protected def write(adr: Addr, v: Value): TaintEvalM[Unit] =
            for
                iTaint <- getImplicitTaint
                res <- super.write(adr, lattice.addAddresses(v, iTaint))
            yield res

        /**
         * Evaluation of a conditional that handles implicit value flows.
         * @note
         *   See [Liu et al. 2010].
         */
        override protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
            for
                prdVal <- eval(prd)
                iTaint <- getImplicitTaint
                // Implicit flows go from the predicate to the branches of the conditional.
                adr = lattice.getAddresses(prdVal) ++ iTaint
                resVal <- withImplicitTaint(adr) {
                    cond(prdVal, eval(csq), eval(alt))
                }
            // Implicit flows need to be added to the return value of the if as well, as this value depends on the predicate.
            yield lattice.addAddresses(resVal, adr)

        override protected def applyClosuresM(
            fun: Value,
            args: List[(SchemeExp, Value)],
            cll: Position,
            ctx: ContextBuilder = DefaultContextBuilder,
          ): M[Value] =
            val arity = args.length
            val closures = lattice.getClosures(fun)
            val explicitFlows = lattice.getAddresses(fun) // Added TODO is this needed?
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
                                iTaint <- getImplicitTaint
                                result = call(targetCmp) // TODO make sure the implicit flows are added to the component even when bottom is returned!
                                updatedResult <- afterCall(result, targetCmp, cll)
                            yield
                                // TODO should this be moved up? If call results bottom, this will otherwise not be executed, or only after a reanalysis, which may trigger further reanalysis.
                                val old = implicitFlowsCut.getOrElse(targetCmp, Set())
                                implicitFlowsCut = // FIXME shouldn't this also contain the cut flows to the current component?
                                    implicitFlowsCut + (targetCmp -> (implicitFlowsCut.getOrElse(targetCmp, Set()) ++ explicitFlows ++ iTaint)) // Added
                                if old != implicitFlowsCut.getOrElse(targetCmp, Set()) then addToWorkList(targetCmp)
                                updatedResult
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
                                iTaint <- getImplicitTaint
                                result = call(targetCmp)
                                updatedResult <- afterCall(result, targetCmp, cll)
                            yield
                                val old = implicitFlowsCut.getOrElse(targetCmp, Set())
                                implicitFlowsCut = // FIXME shouldn't this also contain the cut flows to the current component?
                                    implicitFlowsCut + (targetCmp -> (implicitFlowsCut.getOrElse(targetCmp, Set()) ++ explicitFlows ++ iTaint)) // Added
                                if old != implicitFlowsCut.getOrElse(targetCmp, Set()) then addToWorkList(targetCmp)
                                updatedResult
                        else baseEvalM.fail(VarArityError(cll, prs.length, arity))
                    case _ => Monad[M].unit(lattice.bottom)
                })
            )

    override def intraAnalysis(cmp: Component): SchemeModFBigStepTaintIntra with GlobalStoreTaintIntra

    override def configString(): String = super.configString() + "\n  applying incremental big-step ModF Scheme semantics"
