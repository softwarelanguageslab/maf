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


// a version of EvalM that saves the last control node passed
object TSlicerEvalM: 
                                  
    case class SlicerEvalM[+X](run: Environment[Address] => Option[X]):
       def flatMap[Y](f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = SlicerEvalM(env => run(env).flatMap(res => f(res).run(env)))
       def map[Y](f: X => Y): SlicerEvalM[Y] = SlicerEvalM(env => run(env).map(res => f(res))) 

    trait MonadSlicerEvalM extends TEvalM[SlicerEvalM]:
        def map[X, Y](m: SlicerEvalM[X])(f: X => Y): SlicerEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: SlicerEvalM[X])(f: X => SlicerEvalM[Y]): SlicerEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): SlicerEvalM[X] = SlicerEvalM(_ => Some(x))
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
        def getEnv: SlicerEvalM[Environment[Address]] = SlicerEvalM((env) => Some(env))
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => SlicerEvalM[X]): SlicerEvalM[X] = 
            SlicerEvalM(env => ev.run(f(env)))  
        def merge[X: Lattice](x: SlicerEvalM[X], y: SlicerEvalM[X]): SlicerEvalM[X] = SlicerEvalM { env =>
            (x.run(env), y.run(env)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1, res2))
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
            eval(fnBody).run(fnEnv).foreach((res) => writeResult(res))

        override def eval(exp: SchemeExp): SlicerEvalM[Value] = 
           super.eval(exp)

     
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
