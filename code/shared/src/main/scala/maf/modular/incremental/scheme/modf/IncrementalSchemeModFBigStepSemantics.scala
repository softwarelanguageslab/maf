package maf.modular.incremental.scheme.modf

import maf.core.IdentityMonad
import maf.language.change.CodeVersion.*
import maf.core.*
import maf.language.scheme.*
import maf.modular.incremental.IncrementalGlobalStore
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.modf
import maf.modular.scheme.modf.*
import maf.util.benchmarks.Timeout

/** Implements big-step semantics for an incremental Scheme analysis. * */
trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemanticsT with IncrementalSchemeSemantics with IncrementalGlobalStore[SchemeExp]:

    case class IEvalM[+X](run: (Environment[Addr], List[Set[Addr]]) => Option[X]):
        def map[Y](f: X => Y): IEvalM[Y] = IEvalM((env, addr) => run(env, addr).map(f))
        def flatMap[Y](f: X => IEvalM[Y]): IEvalM[Y] = IEvalM((env, addr) => run(env, addr).flatMap(res => f(res).run(env, addr))) // TODO Are these the right addresses?

    object IEvalM extends TEvalM[IEvalM]:
        def map[X, Y](m: IEvalM[X])(f: X => Y): IEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: IEvalM[X])(f: X => IEvalM[Y]): IEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): IEvalM[X] = IEvalM((_, _) => Some(x))
        def mzero[X]: IEvalM[X] = IEvalM((_, _) => None)
        def guard(cnd: Boolean): IEvalM[Unit] = if cnd then IEvalM((_, _) => Some(())) else mzero
        def withAddr[X](a: Set[Addr])(ev: => IEvalM[X]): IEvalM[X] = IEvalM((env, addr) => ev.run(env, a :: addr))
        def getAddr[X](m: IEvalM[X]): IEvalM[_] = IEvalM((env, addr) => Some(addr))
        def getEnv: IEvalM[Environment[Address]] = IEvalM((env, addr) => Some(env))
        // TODO: withExtendedEnv would make more sense
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => IEvalM[X]): IEvalM[X] =
          IEvalM((env, addr) => ev.run(f(env), addr))
        def merge[X: Lattice](x: IEvalM[X], y: IEvalM[X]): IEvalM[X] = IEvalM { (env, addr) =>
          (x.run(env, addr), y.run(env, addr)) match
              case (None, yres)             => yres
              case (xres, None)             => xres
              case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1, res2))

        }
        def assignVals[X](bds: List[(Identifier, X)], assign: (List[(Identifier, X)], Env) => Unit): EvalM[Unit] =
          IEvalM { (env, addr) =>
              val a: Set[Addr] = addr.flatten.toSet
              Some(assign(bds.map(b => (b._1, lattice.addAddresses(b._2.asInstanceOf[Value], a).asInstanceOf[X])), env))
          }
        implicit class MonadicOps[X](xs: Iterable[X]):
            def foldLeftM[Y](y: Y)(f: (Y, X) => IEvalM[Y]): IEvalM[Y] = xs match
                case Nil     => unit(y)
                case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
            def mapM[Y](f: X => IEvalM[Y]): IEvalM[List[Y]] = xs match
                case Nil => unit(Nil)
                case x :: xs =>
                  for
                      fx <- f(x)
                      rest <- xs.mapM(f)
                  yield fx :: rest
            def mapM_(f: X => IEvalM[Unit]): IEvalM[Unit] = xs match
                case Nil     => unit(())
                case x :: xs => f(x).flatMap(_ => xs.mapM_(f))
    end IEvalM

    type EvalM[X] = IEvalM[X]
    override val evalM: IEvalM.type = IEvalM
    import evalM._

    trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntraT with IncrementalIntraAnalysis:
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

        override protected def evalIf(
            prd: SchemeExp,
            csq: SchemeExp,
            alt: SchemeExp
          ): EvalM[Value] =
          for
              prdVal <- eval(prd)
              adr = lattice.getAddresses(prdVal)
              resVal <- cond(prdVal, withAddr(adr)(eval(csq)), withAddr(adr)(eval(alt)))
          yield resVal

        override def analyzeWithTimeout(timeout: Timeout.T): Unit =
          eval(fnBody).run(fnEnv, List()).foreach(res => writeResult(res))

    override def intraAnalysis(cmp: Component): IncrementalSchemeModFBigStepIntra
