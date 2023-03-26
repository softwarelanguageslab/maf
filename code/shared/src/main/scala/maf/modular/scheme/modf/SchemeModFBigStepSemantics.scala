package maf.modular.scheme.modf

import maf.core._
import maf.core.Monad.MonadSyntaxOps
import maf.core.Position._
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout

/* Generic trait for big-step computations in a "monadic" style */
trait TEvalM[M[_]] extends BaseEvalM[M]:
    /** Implementation of MonadJoin */
    def mbottom[X]: M[X] = mzero
    def mjoin[X: Lattice](x: M[X], y: M[X]): M[X] = merge(x, y)

    /** End implementation of MonadJoin */
    def mzero[X]: M[X]
    /* Guards the execution on the given Boolean variable */
    //def guard(cnd: Boolean): M[Unit]
    def getEnv: M[Environment[Address]]
    // TODO: withExtendedEnv would make more sense
    def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => M[X]): M[X]
    def withEnvM[X](f: Environment[Address] => M[Environment[Address]])(ev: M[X]): M[X] =
        given Monad[M] = this
        for
            newEnv <- Monad[M].flatMap(getEnv)(f)
            result <- withEnv(_ => newEnv) { ev }
        yield result

    //def inject[X: Lattice](x: X): M[X] = if Lattice[X].isBottom(x) then mzero else unit(x)
    def merge[X: Lattice](x: M[X], y: M[X]): M[X]
    def merge[X: Lattice](xs: Iterable[M[X]]): M[X] =
        xs.foldLeft[M[X]](mzero)((acc, x) => merge(acc, x))
    def merge[X: Lattice](xs: M[X]*): M[X] =
        merge(xs)

trait BigStepModFSemanticsT extends BaseSchemeModFSemantics:
    import maf.core.Monad.{MonadIterableOps, MonadSyntaxOps}
    type EvalM[_]
    type M[X] = EvalM[X]
    implicit val evalM: TEvalM[EvalM]
    implicit lazy val baseEvalM = evalM

    import evalM._

    // helper
    protected def cond(
        prd: Value,
        csq: => EvalM[Value],
        alt: => EvalM[Value]
      ): EvalM[Value] =
        val csqValue = guard(lattice.isTrue(prd)).flatMap(_ => csq)
        val altValue = guard(lattice.isFalse(prd)).flatMap(_ => alt)
        merge(csqValue, altValue)

    // defining the intra-analysis
    override def intraAnalysis(cmp: Component): BigStepModFIntraT
    trait BigStepModFIntraT extends IntraAnalysis with SchemeModFSemanticsIntra:
        import evalM._
        // simple big-step eval
        protected def eval(exp: SchemeExp): EvalM[Value] =
            exp match
                case SchemeValue(value, _)              => evalLiteralValue(value, exp)
                case lambda: SchemeLambdaExp            => evalClosure(lambda)
                case SchemeVar(nam)                     => evalVariable(nam)
                case SchemeBegin(exps, _)               => evalSequence(exps)
                case SchemeSet(id, vexp, _)             => evalSet(id, vexp)
                case SchemeIf(prd, csq, alt, _)         => evalIf(prd, csq, alt)
                case SchemeLet(bindings, body, _)       => evalLet(bindings, body)
                case SchemeLetStar(bindings, body, _)   => evalLetStar(bindings, body)
                case SchemeLetrec(bindings, body, _)    => evalLetRec(bindings, body)
                case call @ SchemeFuncall(fun, args, _) => evalCall(call, fun, args)
                case SchemeAssert(exp, _)               => evalAssert(exp)
                case _                                  => throw new Exception(s"Unsupported Scheme expression: $exp")
        protected def evalVariable(id: Identifier): EvalM[Value] =
            getEnv.flatMap(env => lookup(id, env))
        protected def evalClosure(lam: SchemeLambdaExp): EvalM[Value] =
            for env <- getEnv yield newClosure(lam, env)
        protected def evalSequence(exps: List[SchemeExp]): EvalM[Value] =
            Monad.sequence(exps.map(eval)).map(_.last)
        //exps.foldLeftM(lattice.void)((_, exp) => eval(exp))
        private def evalSet(id: Identifier, exp: SchemeExp): EvalM[Value] =
            for
                rhs <- eval(exp)
                env <- getEnv
                _ <- assign(id, env, rhs)
            yield lattice.void
        protected def evalIf(
            prd: SchemeExp,
            csq: SchemeExp,
            alt: SchemeExp
          ): EvalM[Value] =
            for
                prdVal <- eval(prd)
                resVal <- cond(prdVal, eval(csq), eval(alt))
            yield resVal
        protected def evalLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            for
                bds <- bindings.mapM { case (id, exp) => eval(exp).map(vlu => (id, vlu)) }
                res <- withEnvM(env => bind(bds, env)) {
                    evalSequence(body)
                }
            yield res
        protected def evalLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            bindings match
                case Nil => evalSequence(body)
                case (id, exp) :: restBds =>
                    eval(exp).flatMap { rhs =>
                        withEnvM(env => bind(id, env, rhs)) {
                            evalLetStar(restBds, body)
                        }
                    }
        protected def evalLetRec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            withEnvM(env => bindings.foldLeftM(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }) {
                for
                    extEnv <- getEnv
                    _ <- bindings.mapM_ { case (id, exp) =>
                        eval(exp).flatMap(value => assign(id, extEnv, value))
                    }
                    res <- evalSequence(body)
                yield res
            }

        protected def evalAssert(exp: SchemeExp): EvalM[Value] =
            // Assertions are not evaluated by default
            unit(lattice.void)

        protected def evalCall(
            exp: SchemeFuncall,
            fun: SchemeExp,
            args: List[SchemeExp]
          ): EvalM[Value] =
            for
                funVal <- eval(fun)
                argVals <- args.mapM(eval)
                returned <- applyFun(exp, funVal, args.zip(argVals), fun.idn.pos)
                result <- inject(returned)
            yield result

/** A collection of possible configurations of the EvalM monad */
object TEvalM:
    case class EvalM[+X](run: Environment[Address] => Option[X]):
        def flatMap[Y](f: X => EvalM[Y]): EvalM[Y] = EvalM(env => run(env).flatMap(res => f(res).run(env)))
        def map[Y](f: X => Y): EvalM[Y] = EvalM(env => run(env).map(f))

    /** Regular EvalM: joins non-deterministic paths using the analysis lattice, throws an exception if an operation fails */
    trait MonadEvalM extends TEvalM[EvalM]:
        def map[X, Y](m: EvalM[X])(f: X => Y): EvalM[Y] = m.map(f)
        def flatMap[X, Y](m: EvalM[X])(f: X => EvalM[Y]): EvalM[Y] = m.flatMap(f)
        def unit[X](x: X): EvalM[X] = EvalM(_ => Some(x))
        def mzero[X]: EvalM[X] = EvalM(_ => None)
        //def guard(cnd: Boolean): EvalM[Unit] = if cnd then EvalM(_ => Some(())) else mzero
        // TODO: Scala probably already has something for this?
        implicit class MonadicOps[X](xs: Iterable[X]):
            def foldLeftM[Y](y: Y)(f: (Y, X) => EvalM[Y]): EvalM[Y] = xs match
                case Nil     => unit(y)
                case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
            def mapM[Y](f: X => EvalM[Y]): EvalM[List[Y]] = xs match
                case Nil => unit(Nil)
                case x :: xs =>
                    for
                        fx <- f(x)
                        rest <- xs.mapM(f)
                    yield fx :: rest
            def mapM_(f: X => EvalM[Unit]): EvalM[Unit] = xs match
                case Nil     => unit(())
                case x :: xs => f(x).flatMap(_ => xs.mapM_(f))
        def getEnv: EvalM[Environment[Address]] = EvalM(env => Some(env))
        // TODO: withExtendedEnv would make more sense
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] =
            EvalM(env => ev.run(f(env)))
        def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = EvalM { env =>
            (x.run(env), y.run(env)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1, res2))
        }

        def fail[X](e: Error): EvalM[X] =
            throw new Exception(e.toString)

    /** Instead of throwing an error, fails with bottom so that evaluation stops at that point in the analysis */
    trait FailSilentMonadEvalM extends MonadEvalM:
        override def fail[X](e: Error): EvalM[X] =
            mzero

    /** Same as "FailSilent" but prints a warning first */
    trait LogFailMonadEValM extends FailSilentMonadEvalM:
        def warn(msg: String): Unit
        override def fail[X](e: Error): EvalM[X] =
            //warn(s"ignoring error $e")
            super.fail(e)

trait BigStepModFSemantics extends BigStepModFSemanticsT { outer =>
    import TEvalM.{EvalM as BaseEvalM, *}

    object BaseEvalM extends LogFailMonadEValM:
        def warn(msg: String) = outer.warn(msg)

    override type EvalM[X] = BaseEvalM[X]

    implicit val evalM = BaseEvalM
    override def intraAnalysis(component: Component): BigStepModFIntra

    trait BigStepModFIntra extends BigStepModFIntraT {
        // analysis entry point
        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(fnEnv).foreach(res => writeResult(res))
    }

    override def configString(): String = super.configString() + "\n  applying big-step ModF Scheme semantics"
}
