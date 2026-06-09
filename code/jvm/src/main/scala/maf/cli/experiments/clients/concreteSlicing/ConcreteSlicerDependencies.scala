package maf.cli.experiments.clients.concreteSlicing

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.ModAnalysis
import maf.core.Identifier
import maf.language.scheme.SchemeExp
import maf.core.Identity

import maf.modular.scheme.modf.SchemeModFSemanticsM
import maf.modular.scheme.modf.BigStepModFSemantics
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.language.symbolic.lattices.SymbolicSchemeConstantPropagationDomain
import maf.modular.scheme.modf.TEvalM

import maf.core._
import maf.core.Monad.MonadSyntaxOps
import maf.core.Position._
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout
import maf.language.scheme.LexicalRef
import maf.modular.scheme.modflocal.SchemeSemantics

object TSlicerEvalM: 
                               //run: env, defs, controlLoc => value, deps, defs   
    case class SlicerEvalM[+X](run: (Environment[Address], Map[Address, Set[DefLoc]], Option[DefLoc]) => (Option[(X, Set[Address], Map[Address, Set[DefLoc]])])):
       def flatMap[Y](f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = SlicerEvalM((env, defs, ctrl) => run(env, defs, ctrl).flatMap((res, deps, defs2) => f(res).run(env, defs2, ctrl)))
       def map[Y](f: X => Y): SlicerEvalM[Y] = SlicerEvalM((env, defs, ctrl) => run(env, defs, ctrl).map((res, deps, defs2) => (f(res), deps, defs2)))
       def withFilter(p: X => Boolean): SlicerEvalM[X] = SlicerEvalM((env, defs, ctrl) =>
        run(env, defs, ctrl) match 
            case None => None 
            case Some((x, deps, defs2)) => 
                if p(x) then Some((x, deps, defs2))
                        else None
        )
        // DEPENDENCIES
        def deps: SlicerEvalM[(X, Set[Address])] = SlicerEvalM((env, defs, ctrl) => 
            run(env, defs, ctrl) match
                case None => None 
                case Some((x, deps, defs2)) => 
                    Some(((x, deps), deps, defs2))
            )

    trait MonadSlicerEvalM extends TEvalM[SlicerEvalM]:
        def map[X, Y](m: SlicerEvalM[X])(f: X => Y): SlicerEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: SlicerEvalM[X])(f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): SlicerEvalM[X] = SlicerEvalM((_, defs, _) => Some(x, Set.empty, defs))
        def unitWithDeps[X](x: X, deps: Set[Address]): SlicerEvalM[X] = SlicerEvalM((_, defs, _) => Some(x, deps, defs))
        def unitWithDef[X](x: X, addr: Address, loc: DefLoc): SlicerEvalM[X] = unitWithDepsDef(x)(Set.empty)(addr, loc)
        def unitWithDepsDef[X](x: X)(deps: Set[Address])(addr: Address, loc: DefLoc) = SlicerEvalM((_, defs, _) =>
            val oldLocs = defs.getOrElse(addr, Set.empty)
            Some(x, deps, defs + (addr -> (oldLocs + loc))))
        def mzero[X]: SlicerEvalM[X] = SlicerEvalM((_, _, _) => None)
        implicit class MonadicOps[X](xs: Iterable[X]):
            def foldLeftM[Y](y: Y)(f: (Y, X) => SlicerEvalM[Y]): SlicerEvalM[Y] = xs match
                case Nil     => unit(y)
                case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
            def mapM[Y](f: X => SlicerEvalM[Y]): SlicerEvalM[List[Y]] = xs match
                case Nil => unit(Nil)
                case x :: xs =>
                    for
                        fx <- f(x)
                        rest <- xs.mapM(f)
                    yield fx :: rest
            def mapM_(f: X => SlicerEvalM[Unit]): SlicerEvalM[Unit] = xs match
                case Nil     => unit(())
                case x :: xs => f(x).flatMap(_ => xs.mapM_(f))  
        def getEnv: SlicerEvalM[Environment[Address]] = SlicerEvalM((env, defs, _) => Some(env, Set.empty, defs))
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, defs, ctrl) => ev.run(f(env), defs, ctrl))  
        def merge[X: Lattice](x: SlicerEvalM[X], y: SlicerEvalM[X]): SlicerEvalM[X] = SlicerEvalM { (env, defs, ctrl) =>
            (x.run(env, defs, ctrl), y.run(env, defs, ctrl)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some((res1, deps1, defs1)), Some((res2, deps2, defs2))) => Some((Lattice[X].join(res1, res2), deps1 ++ deps2, defs1 ++ defs2))
        }
        def fail[X](err: Error): SlicerEvalM[X] = mzero
        // DEFINITIONS
        def getDefs: SlicerEvalM[Map[Address, Set[DefLoc]]] = SlicerEvalM((_, defs, _) => Some(defs, Set.empty, defs))
        def withDefs[X](f: Map[Address, Set[DefLoc]] => Map[Address, Set[DefLoc]])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, defs, ctrl) => ev.run(env, f(defs), ctrl))  
        // CONTROL DEPS
        def getCtrl: SlicerEvalM[Option[DefLoc]] = SlicerEvalM((_, defs, ctrl) => Some(ctrl, Set.empty, defs))
        def withCtrl[X](ctrl: DefLoc)(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, defs, _) => ev.run(env, defs, Some(ctrl))) 

case class DefLoc(loc: Identity, index: Option[Int])

trait ConcreteSlicerDependencies extends BigStepModFSemanticsT:
    import TSlicerEvalM.{*}

    object SlicerEvalM extends MonadSlicerEvalM

    override type EvalM[X] = SlicerEvalM[X] 
    implicit val evalM = SlicerEvalM
    val controlEvalM: MonadSlicerEvalM = SlicerEvalM 

    var finalDefs: Map[Address, Set[DefLoc]] = Map.empty
    var finalDeps: Map[SchemeExp, Set[Address]] = Map.empty
    var finalControlDeps: Map[SchemeExp, Option[DefLoc]] = Map.empty

    override def intraAnalysis(cmp: Component): ConcreteSlicerDependenciesIntra 
    trait ConcreteSlicerDependenciesIntra extends IntraAnalysis with BigStepModFIntraT: 
        import controlEvalM._

        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(fnEnv, Map.empty, None).foreach((res, deps, defs) => 
                //writeResult(res)
                finalDefs = defs
                )   

        override def eval(exp: SchemeExp): SlicerEvalM[Value] = 
            for 
                (res, deps) <- evalWithIdentity(exp).deps
                result <- unitWithDeps(res, deps)
                ctrlDep <- getCtrl
            yield 
                finalDeps = finalDeps + (exp -> deps)
                finalControlDeps = finalControlDeps + (exp -> ctrlDep)
                result

        def evalWithIdentity(exp: SchemeExp): SlicerEvalM[Value] = 
            exp match
                case SchemeSet(id, vexp, idt)             => evalSet(id, vexp, idt)
                case SchemeSetLex(id, _, vexp, idt)       => evalSet(id, vexp, idt)
                case SchemeLet(bindings, body, idt)       => evalLet(bindings, body, idt)
                case SchemeLetStar(bindings, body, idt)   => evalLetStar(bindings, body, idt, 0)
                case SchemeLetrec(bindings, body, idt)    => evalLetRec(bindings, body, idt)
                case call @ SchemeFuncall(fun, args, idt) => evalCall(call, fun, args, idt)
                case SchemeIf(prd, csq, alt, idt)         => evalIf(prd, csq, alt, idt)
                case _                                    => super.eval(exp)
        
        // LAMBDAS
        // todo: save the defs in the lambda environment (lexical scoping)

        // SEQUENCES
        override protected def evalSequence(exps: List[SchemeExp]): EvalM[Value] =
            for 
                evalled <- exps.mapM(exp => eval(exp).deps)
                deps = evalled.map(_._2)
                values = evalled.map(_._1) 
                res <- unitWithDeps(values.last, deps.last)
            yield res

        // ASSIGNMENTS
        protected def evalSet(id: Identifier, exp: SchemeExp, idt: Identity): EvalM[Value] =
            for
                (rhs, rhsDeps) <- eval(exp).deps
                env <- getEnv
                _ <- assign(id, env, rhs)
                addr = env.lookup(id.name).get // get should not be a problem here because assign will throw an error if it was None
                res <- unitWithDepsDef(lattice.void)(rhsDeps)(addr, DefLoc(idt, None))
            yield lattice.void
                

        // IF EXPRESSIONS
        // todo: keep track of control node
        protected def evalIf(
            prd: SchemeExp,
            csq: SchemeExp,
            alt: SchemeExp,
            idt: Identity
          ): EvalM[Value] =
            for
                (prdVal, prdDeps) <- eval(prd).deps
                (resVal, resDeps) <- withCtrl(DefLoc(idt, None)){ cond(prdVal, eval(csq), eval(alt)).deps }
                res <- unitWithDeps(resVal, resDeps ++ prdDeps)
            yield res

        // LET EXPRESSIONS
        protected def evalLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], idt: Identity): EvalM[Value] =
            for
                bds <- bindings.mapM { case (id, exp) => eval(exp).deps.map((vlu, deps) => ((id, vlu), deps)) }
                boundAddrs = bds.map((bd, _) => allocVar(bd._1, component))
                locs = for ((addr, idx) <- boundAddrs.zipWithIndex) yield (addr, Set(DefLoc(idt, Some(idx))))
                (value, deps) <- withEnvM(env => bind(bds.map(_._1), env)) {
                    // update the defs
                    withDefs(defs => defs ++ locs.toMap) {
                        evalSequence(body).deps
                    }
                }
                // only keep dependencies that are not defined by the let itself
                filteredDeps = deps.filter(addr => !boundAddrs.contains(addr))
                // the final dependencies also includes the dependencies of the bindings
                // TODO: dependencies only of relevant bindings
                res <- unitWithDeps(value, filteredDeps ++ bds.map(_._2).fold(Set.empty)((x, y) => x ++ y))
            yield value
        
        protected def evalLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], idt: Identity, idx: Int): EvalM[Value] =
            bindings match
                case Nil => evalSequence(body)
                case (id, exp) :: restBds =>
                    eval(exp).deps.flatMap { (rhs, currDeps) =>
                        withEnvM(env => bind(id, env, rhs)) {
                            val boundAddr = allocVar(id, component)
                            withDefs(defs => defs + (boundAddr -> Set(DefLoc(idt, Some(idx))))) {
                                for 
                                    (value, restDeps) <- evalLetStar(restBds, body, idt, idx + 1).deps
                                    restDepsFiltered = restDeps.filter(d => d != boundAddr)
                                    res <- unitWithDeps(value, currDeps ++ restDepsFiltered)
                                yield res
                            }
                        }
                    }
        protected def evalLetRec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], idt: Identity): EvalM[Value] =
            withEnvM(env => bindings.foldLeftM(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }) {
                val boundAddrs = bindings.map((id, _) => allocVar(id, component))
                val locs = for ((addr, idx) <- boundAddrs.zipWithIndex) yield (addr, Set(DefLoc(idt, Some(idx))))
                withDefs(defs => defs ++ locs.toMap) {
                    for
                        extEnv <- getEnv
                        bindingDeps <- bindings.mapM { case (id, exp) =>
                            for 
                                (bindingValue, bindingDep) <- eval(exp).deps
                                _ <- assign(id, extEnv, bindingValue)
                            yield bindingDep
                        }
                        (bodyRes, bodyDeps) <- evalSequence(body).deps 
                        filteredDeps = (bodyDeps ++ bindingDeps.flatten).filter(d => ! boundAddrs.contains(d))
                        res <- unitWithDeps(bodyRes, filteredDeps)
                    yield res
                }
            }

        // FUNCTION CALLS
        protected def evalCall(
            exp: SchemeFuncall,
            fun: SchemeExp,
            args: List[SchemeExp],
            idt: Identity
          ): EvalM[Value] =
            for
                (funVal, funDeps) <- eval(fun).deps
                argsEvalled <- args.mapM(arg => eval(arg).deps)
                argVals = argsEvalled.map(_._1)
                argDeps = argsEvalled.map(_._2)
                returned <- applyFun(exp, funVal, args.zip(argVals), fun.idn.pos)
                result <- inject(returned)
                res <- unitWithDeps(result, funDeps ++ argDeps.flatten)
            yield res

        // VARIABLES
        override protected def lookup(id: Identifier, env: Env): SlicerEvalM[Value] = 
            env.lookup(id.name) match
                case None       => baseEvalM.fail(UndefinedVariableError(id))
                case Some(addr) => 
                    unitWithDeps(readAddr(addr), Set(addr))

     
object ConcreteSlicerDependencies:
    type Analysis = ConcreteSlicerDependencies

    def createAnalysis(program: SchemeExp): ConcreteSlicerDependencies =
        new ModAnalysis[SchemeExp](program)
            with StandardSchemeModFComponents
            with SchemeModFSemanticsM
            with SchemeModFNoSensitivity
            with SymbolicSchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with ConcreteSlicerDependencies:

            class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with ConcreteSlicerDependenciesIntra
            override def intraAnalysis(cmp: Component): AnalysisIntra =
                new AnalysisIntra(cmp)
