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
                                  
    case class SlicerEvalM[+X](run: Environment[Address] => (Option[(X, Set[Address])])):
       def flatMap[Y](f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = SlicerEvalM(env => run(env).flatMap((res, defs) => f(res).run(env)))
       def map[Y](f: X => Y): SlicerEvalM[Y] = SlicerEvalM(env => run(env).map((res, defs) => (f(res), defs)))
       def withFilter(p: X => Boolean): SlicerEvalM[X] = SlicerEvalM(env =>
        run(env) match 
            case None => None 
            case Some((x, deps)) => 
                if p(x) then Some((x, deps))
                        else None
        )
        def deps: SlicerEvalM[(X, Set[Address])] = SlicerEvalM(env => 
            run(env) match
                case None => None 
                case Some((x, deps)) => 
                    Some(((x, deps), deps))
            )

    trait MonadSlicerEvalM extends TEvalM[SlicerEvalM]:
        def map[X, Y](m: SlicerEvalM[X])(f: X => Y): SlicerEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: SlicerEvalM[X])(f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): SlicerEvalM[X] = SlicerEvalM(_ => Some(x, Set.empty))
        def unitWithDeps[X](x: X, deps: Set[Address]): SlicerEvalM[X] = SlicerEvalM(_ => Some(x, deps))
        def mzero[X]: SlicerEvalM[X] = SlicerEvalM((_) => None)
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
        def getEnv: SlicerEvalM[Environment[Address]] = SlicerEvalM((env) => Some(env, Set.empty))
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM(env => ev.run(f(env)))  
        def merge[X: Lattice](x: SlicerEvalM[X], y: SlicerEvalM[X]): SlicerEvalM[X] = SlicerEvalM { env =>
            (x.run(env), y.run(env)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some((res1, defs1)), Some((res2, defs2))) => Some((Lattice[X].join(res1, res2), defs1 ++ defs2))
        }
        def fail[X](err: Error): SlicerEvalM[X] = mzero
        

trait ConcreteSlicer extends BigStepModFSemanticsT:
    import TSlicerEvalM.{*}

    object SlicerEvalM extends MonadSlicerEvalM

    override type EvalM[X] = SlicerEvalM[X] 
    implicit val evalM = SlicerEvalM
    val controlEvalM: MonadSlicerEvalM = SlicerEvalM 

    override def intraAnalysis(cmp: Component): ConcreteSlicerIntra 
    trait ConcreteSlicerIntra extends IntraAnalysis with BigStepModFIntraT: 
        import controlEvalM._

        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(fnEnv).foreach((res, deps) => 
                println("final res: " + res)
                println("final deps: " + deps)
                // writeResult(res)
                )

        override def eval(exp: SchemeExp): SlicerEvalM[(Value)] = 
            for 
                (res, deps) <- super.eval(exp).deps
                _ = println("expression: " + exp)
                _ = println("deps: " + deps)
            yield res

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
                res <- SlicerEvalM.unitWithDeps(result, funDeps ++ argDeps.flatten)
            yield res

        override protected def lookup(id: Identifier, env: Env): SlicerEvalM[Value] = 
            env.lookup(id.name) match
                case None       => baseEvalM.fail(UndefinedVariableError(id))
                case Some(addr) => 
                    SlicerEvalM.unitWithDeps(readAddr(addr), Set(addr))

     
object ConcreteSlicer:
    type Analysis = ConcreteSlicer

    def createAnalysis(program: SchemeExp): ConcreteSlicer =
        new ModAnalysis[SchemeExp](program)
            with StandardSchemeModFComponents
            with SchemeModFSemanticsM
            with SchemeModFNoSensitivity
            with SymbolicSchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with ConcreteSlicer:

            class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with ConcreteSlicerIntra
            override def intraAnalysis(cmp: Component): AnalysisIntra =
                new AnalysisIntra(cmp)
