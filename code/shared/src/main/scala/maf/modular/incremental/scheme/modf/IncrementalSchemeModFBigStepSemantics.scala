package maf.modular.incremental.scheme.modf

import maf.core.IdentityMonad
import maf.language.change.CodeVersion.*
import maf.core.*
import maf.core.Position.Position
import maf.language.scheme.*
import maf.language.sexp
import maf.modular.incremental.IncrementalGlobalStoreCY
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.scheme.modf.SchemeModFComponent.Call
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.core.Monad.MonadSyntaxOps

/** Implements big-step semantics for an incremental Scheme analysis. * */
trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemanticsT with IncrementalSchemeSemantics with IncrementalGlobalStoreCY[SchemeExp]:

    type EvalM[X] = FlowEvalM[X]
    implicit val evalM: MonadFlowEvalM = new MonadFlowEvalM {}

    case class FlowEvalM[+X](run: (Set[Address], Environment[Address]) => Option[X]):
        def flatMap[Y](f: X => FlowEvalM[Y]): FlowEvalM[Y] =
            FlowEvalM((implicitFlows, env) => run(implicitFlows, env).flatMap(res => f(res).run(implicitFlows, env)))
        def map[Y](f: X => Y): FlowEvalM[Y] = FlowEvalM((implicitFlows, env) => run(implicitFlows, env).map(f))
        def fail[X](e: Error): EvalM[X] = throw new Exception(e.toString)

    trait MonadFlowEvalM extends TEvalM[FlowEvalM] :
        def map[X, Y](m: FlowEvalM[X])(f: X => Y): FlowEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: FlowEvalM[X])(f: X => FlowEvalM[Y]): FlowEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): FlowEvalM[X] = FlowEvalM((_, _) => Some(x))
        def mzero[X]: FlowEvalM[X] = FlowEvalM((_, _) => None)
        //def guard(cnd: Boolean): TaintEvalM[Unit] = if cnd then TaintEvalM(_ => Some(())) else mzero
        // TODO: Scala probably already has something for this?
        implicit class MonadicOps[X](xs: Iterable[X]):
            def foldLeftM[Y](y: Y)(f: (Y, X) => FlowEvalM[Y]): FlowEvalM[Y] = xs match
                case Nil => unit(y)
                case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
            def mapM[Y](f: X => FlowEvalM[Y]): FlowEvalM[List[Y]] = xs match
                case Nil => unit(Nil)
                case x :: xs =>
                    for
                        fx <- f(x)
                        rest <- xs.mapM(f)
                    yield fx :: rest
            def mapM_(f: X => FlowEvalM[Unit]): FlowEvalM[Unit] = xs match
                case Nil => unit(())
                case x :: xs => f(x).flatMap(_ => xs.mapM_(f))
        def getEnv: FlowEvalM[Environment[Address]] = FlowEvalM((_, env) => Some(env))
        // TODO: withExtendedEnv would make more sense
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => FlowEvalM[X]): FlowEvalM[X] =
            FlowEvalM((imF, env) => ev.run(imF, f(env)))
        def getImplicitFlows: FlowEvalM[Set[Address]] = FlowEvalM((implicitFlows, _) => Some(implicitFlows))
        def withImplicitFlows[X](implicitFlows: Set[Addr])(ev: => FlowEvalM[X]): FlowEvalM[X] = FlowEvalM((_, env) => ev.run(implicitFlows, env))
        def merge[X: Lattice](x: FlowEvalM[X], y: FlowEvalM[X]): FlowEvalM[X] = FlowEvalM { (implicitFlows, env) =>
            (x.run(implicitFlows, env), y.run(implicitFlows, env)) match
                case (None, yres) => yres
                case (xres, None) => xres
                case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1, res2))
        }
        def fail[X](e: Error): FlowEvalM[X] = mzero // throw new Exception(e.toString)
    end MonadFlowEvalM

    override def warn(msg: String): Unit = ()

    //////////////////////////////////////////////////////////////////////////////////////////////

    trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntraT with IncrementalIntraAnalysis with IncrementalGlobalStoreCYIntraAnalysis:
        import evalM._

        var cutFlows: Map[Component, Set[Addr]] = Map().withDefaultValue(Set())
        var litAddrs: Set[Addr] = Set()

        override def commit(): Unit =
            // Update this here. This way it can be decided when reanalysis is needed in applyClosuresM using interComponentFlow.
            interComponentFlow = interComponentFlow + (component -> cutFlows)
            litAddr = litAddr + (component -> litAddrs)
            super.commit()

        // analysis entry point
        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(Set[Addr](), fnEnv).foreach(res => writeResult(res))

       /*  override protected def allocateVal(exp: SchemeExp)(v: Value): M[Value] =
            if configuration.cyclicValueInvalidation then
                val addr = allocPtr(exp, component)
                for
                    _ <- write(addr, v)
                    iFlows <- getImplicitFlows
                    res <- baseEvalM.unit(lattice.addAddress(lattice.pointer(addr), addr))
                yield
                    dataFlow += (addr -> SmartUnion.sunion(dataFlow(addr), iFlows))
                    res
            else super.allocateVal(exp)(v) */

        override protected def eval(exp: SchemeExp): EvalM[Value] = exp match
            case SchemeCodeChange(e, _, _) if version == Old =>
                registerComponent(e, component)
                eval(e) // This could also be a super call if we assume no nesting of change expressions (which could be expected).
            case SchemeCodeChange(_, e, _) if version == New =>
                registerComponent(e, component)
                eval(e) // Same than above.
            case _ =>
                registerComponent(exp, component)
                super.eval(exp)

        // Add the implicit flows to the value written, so the writeAddr in the global store can retrieve them.
        override protected def write(adr: Addr, v: Value): FlowEvalM[Unit] =
            for
                iFlow <- getImplicitFlows
                res <- super.write(adr, lattice.addAddresses(v, iFlow))
            yield res

        /**
         * Evaluation of a conditional that handles implicit value flows.
         * @note
         *   See [Liu et al. 2010].
         */
        override protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
            if configuration.cyclicValueInvalidation then
                for
                    prdVal <- eval(prd)
                    // Implicit flows go from the predicate to the branches of the conditional.
                    // When CY is disabled, no addresses will be present (and implicitFlows will be a list of empty sets).
                    iFlows <- getImplicitFlows
                    adr = lattice.getAddresses(prdVal) ++ iFlows
                    resVal <- withImplicitFlows(adr) {
                        cond(prdVal, eval(csq), eval(alt))
                    }
                // Implicit flows need to be added to the return value of the if as well, as this value depends on the predicate.
                yield lattice.addAddresses(resVal, adr)
            else super.evalIf(prd, csq, alt)

        /** Evaluation of a literal value that adds a "literal address" as source. */
        override protected def evalLiteralValue(literal: sexp.Value, exp: SchemeExp): M[Value] =
            // TODO: add to data flow, or add to provenance? (can't be part of SCC anyway, so best just to add to provenance?)
            // Make it part of an SCA using implicit flows! This fixes precision loss due to conditional reading of literals!
            if configuration.cyclicValueInvalidation then
                val a = LitAddr(exp)
                litAddrs += a
                for
                    iFlows <- getImplicitFlows
                    //_ = { dataFlow += (a -> SmartUnion.sunion(dataFlow(a), iFlows)) } // TODO!! (+ todo: remove iflows from value)
                    value <- super.evalLiteralValue(literal, exp).map(lattice.addAddress(_, a)) // Attach the address to the value for flow tracking + implicit flows!.
                // _ = { if !lattice.isBottom(value) then intraProvenance += (a -> value) } // We can just overwrite any previous value as it will be the same.
                yield
                    dataFlow += (a -> SmartUnion.sunion(dataFlow(a), iFlows)) // TODO!! (+ todo: remove iflows from value)
                    value
            else super.evalLiteralValue(literal, exp)

        override protected def applyClosuresM(fun: Value, args: List[(SchemeExp, Value)], cll: Position, ctx: ContextBuilder = DefaultContextBuilder): M[Value] =
            if configuration.cyclicValueInvalidation
            then
                val arity = args.length
                val closures = lattice.getClosures(fun)
                val explicitFlows = lattice.getAddresses(fun) // Added TODO is this needed?
                MonadJoin[M].mfoldMap(closures)((clo) =>
                    (clo match {
                        case (SchemeLambda(_, prs, _, _, _), _) =>
                            if prs.length == arity then
                                val argVals = args.map(_._2)
                                val argsVNoFlow = argVals.map(lattice.removeAddresses) // Ensure that the flow doesn't alter the context-sensitivity!
                                for
                                    context <- ctx.allocM(clo, argsVNoFlow, cll, component)
                                    targetCall = Call(clo, context)
                                    targetCmp <- newComponentM(targetCall)
                                    _ = bindArgs(targetCmp, prs, argVals)
                                    _ <- ctx.beforeCall(targetCmp, prs, clo)
                                    iTaint <- getImplicitFlows
                                    result = call(targetCmp) // TODO make sure the implicit flows are added to the component even when bottom is returned!
                                    updatedResult <- afterCall(result, targetCmp, cll)
                                yield
                                    // TODO should this be moved up? If call results bottom, this will otherwise not be executed, or only after a reanalysis, which may trigger further reanalysis. => not needed since iflows added afterwards
                                    cutFlows = cutFlows + (targetCmp -> (cutFlows(targetCmp) ++ explicitFlows ++ iTaint)) // TODO: do we need explicit flows here?
                                    updatedResult
                            else baseEvalM.fail(ArityError(cll, prs.length, arity))
                        case (SchemeVarArgLambda(_, prs, vararg, _, _, _), _) =>
                            if prs.length <= arity then
                                val (fixedArgs, varArgs) = args.splitAt(prs.length)
                                val fixedArgVals = fixedArgs.map(_._2)
                                val fixedArgsVNoFlow = fixedArgVals.map(lattice.removeAddresses) // Ensure that the flow doesn't alter the context-sensitivity!
                                for
                                    varArgVal <- allocateList(varArgs)
                                    context <- ctx.allocM(clo, fixedArgsVNoFlow :+ varArgVal, cll, component)
                                    targetCall = Call(clo, context)
                                    targetCmp <- newComponentM(targetCall)
                                    _ = bindArgs(targetCmp, prs, fixedArgVals)
                                    _ = bindArg(targetCmp, vararg, varArgVal)
                                    _ <- ctx.beforeCall(targetCmp, prs, clo)
                                    iTaint <- getImplicitFlows
                                    result = call(targetCmp)
                                    updatedResult <- afterCall(result, targetCmp, cll)
                                yield
                                    cutFlows = cutFlows + (targetCmp -> (cutFlows(targetCmp) ++ explicitFlows ++ iTaint)) // TODO: do we need explicit flows here?
                                    updatedResult
                            else baseEvalM.fail(VarArityError(cll, prs.length, arity))
                        case _ => Monad[M].unit(lattice.bottom)
                    })
                )
            else super.applyClosuresM(fun, args, cll, ctx)
        end applyClosuresM

    override def intraAnalysis(cmp: Component): IncrementalSchemeModFBigStepIntra

    override def configString(): String = super.configString() + "\n  applying incremental big-step ModF Scheme semantics"
