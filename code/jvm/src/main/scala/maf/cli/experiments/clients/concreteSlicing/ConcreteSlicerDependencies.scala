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
                               //run: env, assignments, defs, controlLoc => value, deps, assignments, defs   
    case class SlicerEvalM[+X](run: (Environment[Address], Map[Address, Set[SchemeExp]], Map[Address, SchemeExp], Option[SchemeExp]) => (Option[(X, Set[Address], Map[Address, Set[SchemeExp]], Map[Address, SchemeExp])])):
       def flatMap[Y](f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = SlicerEvalM((env, ass, defs, ctrl) => 
        run(env, ass, defs, ctrl).flatMap((res, deps, ass2, defs2) => f(res).run(env, ass2, defs2, ctrl)))
       def map[Y](f: X => Y): SlicerEvalM[Y] = SlicerEvalM((env, ass, defs, ctrl) => run(env, ass, defs, ctrl).map((res, deps, ass2, defs2) => (f(res), deps, ass2, defs2)))
       def withFilter(p: X => Boolean): SlicerEvalM[X] = SlicerEvalM((env, ass, defs, ctrl) =>
        run(env, ass, defs, ctrl) match 
            case None => None 
            case Some((x, deps, ass2, defs2)) => 
                if p(x) then Some((x, deps, ass2, defs2))
                        else None
        )
        // DEPENDENCIES
        def deps: SlicerEvalM[(X, Set[Address])] = SlicerEvalM((env, ass, defs, ctrl) => 
            run(env, ass, defs, ctrl) match
                case None => None 
                case Some((x, deps, ass2, defs2)) => 
                    Some(((x, deps), deps, ass2, defs2))
            )

    trait MonadSlicerEvalM extends TEvalM[SlicerEvalM]:
        def map[X, Y](m: SlicerEvalM[X])(f: X => Y): SlicerEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: SlicerEvalM[X])(f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): SlicerEvalM[X] = SlicerEvalM((_, ass, defs, _) => Some(x, Set.empty, ass, defs))
        def unitWithDeps[X](x: X, deps: Set[Address]): SlicerEvalM[X] = SlicerEvalM((_, ass, defs, _) => Some(x, deps, ass, defs))
        def addDep(dep: Address): SlicerEvalM[Unit] = SlicerEvalM((_, ass, defs, _) => Some((), Set(dep), ass, defs))
        def unitWithDef[X](x: X, addr: Address, loc: SchemeExp): SlicerEvalM[X] = unitWithDepsDef(x)(Set.empty)(addr, loc)
        def unitWithDepsDef[X](x: X)(deps: Set[Address])(addr: Address, loc: SchemeExp) = SlicerEvalM((_, ass, defs, _) =>
            val oldLocs = ass.getOrElse(addr, Set.empty)
            Some(x, deps, ass + (addr -> (oldLocs + loc)), defs))
        def mzero[X]: SlicerEvalM[X] = SlicerEvalM((_, _, _, _) => None)
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
        def getEnv: SlicerEvalM[Environment[Address]] = SlicerEvalM((env, ass, defs, _) => Some(env, Set.empty, ass, defs))
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, ass, defs, ctrl) => ev.run(f(env), ass, defs, ctrl))  
        def merge[X: Lattice](x: SlicerEvalM[X], y: SlicerEvalM[X]): SlicerEvalM[X] = SlicerEvalM { (env, ass, defs, ctrl) =>
            (x.run(env, ass, defs, ctrl), y.run(env, ass, defs, ctrl)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some((res1, deps1, ass1, defs1)), Some((res2, deps2, ass2, defs2))) => Some((Lattice[X].join(res1, res2), deps1 ++ deps2, ass1 ++ ass2, defs1 ++ defs2))
        }
        def fail[X](err: Error): SlicerEvalM[X] = mzero
        // DEFINITIONS
        def getAss: SlicerEvalM[Map[Address, Set[SchemeExp]]] = SlicerEvalM((_, ass, defs, _) => Some(ass, Set.empty, ass, defs))
        def withAss[X](f: Map[Address, Set[SchemeExp]] => Map[Address, Set[SchemeExp]])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, ass, defs, ctrl) => ev.run(env, f(ass), defs, ctrl))  
        def addDef[X](addr: Address, loc: SchemeExp)(ev: => SlicerEvalM[X]): SlicerEvalM[X] =
            SlicerEvalM((env, ass, defs, ctrl) => ev.run(env, ass, (defs + (addr -> loc)), ctrl))
        def withDefs[X](f: Map[Address, SchemeExp] => Map[Address, SchemeExp])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, ass, defs, ctrl) => ev.run(env, ass, f(defs), ctrl))
        // CONTROL DEPS
        def getCtrl: SlicerEvalM[Option[SchemeExp]] = SlicerEvalM((_, ass, defs, ctrl) => Some(ctrl, Set.empty, ass, defs))
        def withCtrl[X](ctrl: SchemeExp)(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM((env, ass, defs, _) => ev.run(env, ass, defs, Some(ctrl))) 

trait ConcreteSlicerDependencies extends BigStepModFSemanticsT:
    import TSlicerEvalM.{*}

    object SlicerEvalM extends MonadSlicerEvalM

    override type EvalM[X] = SlicerEvalM[X] 
    implicit val evalM = SlicerEvalM
    val controlEvalM: MonadSlicerEvalM = SlicerEvalM 

    var finalAss: Map[Address, Set[SchemeExp]] = Map.empty
    var finalDefs: Map[Address, SchemeExp] = Map.empty
    var finalDeps: Map[SchemeExp, Set[Address]] = Map.empty
    var finalControlDeps: Map[SchemeExp, Option[SchemeExp]] = Map.empty

    override def intraAnalysis(cmp: Component): ConcreteSlicerDependenciesIntra 
    
    var i = 0
    trait ConcreteSlicerDependenciesIntra extends IntraAnalysis with BigStepModFIntraT: 
        import controlEvalM._

        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            i = i + 1
            println("==============")
            println(s"[$i] ANALYSING $component")
            println("==============")
            eval(fnBody).run(fnEnv, Map.empty, Map.empty, None).foreach((res, deps, ass, defs) => 
                writeResult(res)
                finalDefs = defs
                finalAss = ass
            )   

        override def eval(exp: SchemeExp): SlicerEvalM[Value] = 
            for 
                (res, deps) <- evalWithIdentity(exp).deps
                ctrlDep <- getCtrl
                result <- unitWithDeps(res, deps)
            yield 
                finalDeps = finalDeps + (exp -> deps)
                finalControlDeps = finalControlDeps + (exp -> ctrlDep)
                result

        def evalWithIdentity(exp: SchemeExp): SlicerEvalM[Value] = 
            exp match
                case SchemeSet(id, vexp, idt)             => evalSet(id, vexp, exp)
                case SchemeSetLex(id, _, vexp, idt)       => evalSet(id, vexp, exp)
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
        protected def evalSet(id: Identifier, exp: SchemeExp, setExp: SchemeExp): EvalM[Value] =
            for
                (rhs, rhsDeps) <- eval(exp).deps
                env <- getEnv
                _ <- assign(id, env, rhs)
                addr = env.lookup(id.name).get // get should not be a problem here because assign will throw an error if it was None
                res <- unitWithDepsDef(lattice.void)(rhsDeps)(addr, setExp)
            yield res
                

        // IF EXPRESSIONS
        override protected def evalIf(
            prd: SchemeExp,
            csq: SchemeExp,
            alt: SchemeExp
          ): EvalM[Value] =
            for
                (prdVal, prdDeps) <- eval(prd).deps
                (resVal, resDeps) <- withCtrl(prd){ cond(prdVal, eval(csq), eval(alt)).deps }
                res <- unitWithDeps(resVal, resDeps ++ prdDeps)
            yield res

        // LET EXPRESSIONS
        override protected def evalLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            for
                bds <- bindings.mapM { case (id, exp) => eval(exp).deps.map((vlu, deps) => ((id, vlu), deps)) }
                boundAddrs = bindings.map((bd, exp) => (allocVar(bd, component), exp))
                (value, deps) <- withEnvM(env => bind(bds.map(_._1), env)) {
                    // update the defs
                    withDefs(defs => defs ++ boundAddrs.toMap) {
                        evalSequence(body).deps
                    }
                }
                // only keep dependencies that are not defined by the let itself
                filteredDeps = deps.filter(addr => !boundAddrs.map(_._1).contains(addr))
                // the final dependencies also includes the dependencies of the bindings
                // TODO: dependencies only of relevant bindings
                res <- unitWithDeps(value, filteredDeps ++ bds.map(_._2).fold(Set.empty)((x, y) => x ++ y))
            yield value
        
        override protected def evalLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            bindings match
                case Nil => evalSequence(body)
                case (id, exp) :: restBds =>
                    eval(exp).deps.flatMap { (rhs, currDeps) =>
                        withEnvM(env => bind(id, env, rhs)) {
                            val boundAddr = allocVar(id, component)
                            addDef(boundAddr, exp) {
                                for 
                                    (value, restDeps) <- evalLetStar(restBds, body).deps
                                    restDepsFiltered = restDeps.filter(d => d != boundAddr)
                                    currDepsFiltered = currDeps.filter(d => d!= boundAddr)
                                    res <- unitWithDeps(value, currDepsFiltered ++ restDepsFiltered)
                                yield res
                            }
                        }
                    }
        override protected def evalLetRec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            withEnvM(env => bindings.foldLeftM(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }) {
                val boundAddrs = bindings.map((id, exp) => (allocVar(id, component), exp))
                withDefs(defs => defs ++ boundAddrs.toMap) {
                    for
                        extEnv <- getEnv
                        bindingDeps <- bindings.mapM { case (id, exp) =>
                            for 
                                (bindingValue, bindingDep) <- eval(exp).deps
                                _ <- assign(id, extEnv, bindingValue)
                            yield bindingDep
                        }
                        (bodyRes, bodyDeps) <- evalSequence(body).deps 
                        filteredDeps = (bodyDeps ++ bindingDeps.flatten).filter(d => !boundAddrs.map(_._1).contains(d))
                        res <- unitWithDeps(bodyRes, filteredDeps)
                    yield res
                }
            }

        // FUNCTION CALLS
        override protected def evalCall(
            exp: SchemeFuncall,
            fun: SchemeExp,
            args: List[SchemeExp]
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
