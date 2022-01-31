package maf.modular.scheme.modf

import maf.core._
import maf.core.Position._
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout

/* Generic trait for big-step computations in a "monadic" style */
trait TEvalM[M[_]] extends Monad[M]:
    def mzero[X]: M[X]
    /* Guards the execution on the given Boolean variable */
    def guard(cnd: Boolean): M[Unit]
    def getEnv: M[Environment[Address]]
    // TODO: withExtendedEnv would make more sense
    def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => M[X]): M[X]
    def inject[X: Lattice](x: X): M[X] = if Lattice[X].isBottom(x) then mzero else unit(x)
    def merge[X: Lattice](x: M[X], y: M[X]): M[X]
    def merge[X: Lattice](xs: Iterable[M[X]]): M[X] =
      xs.foldLeft[M[X]](mzero)((acc, x) => merge(acc, x))

trait BigStepModFSemanticsT extends BaseSchemeModFSemantics:
    import maf.core.Monad.{MonadIterableOps, MonadSyntaxOps}
    type EvalM[_]
    type M[X] = EvalM[X]
    implicit val evalM: TEvalM[EvalM]
    implicit lazy val baseEvalM: Monad[M] = evalM

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
        protected def eval(exp: SchemeExp): EvalM[Value] = exp match
            case SchemeValue(value, _)              => unit(evalLiteralValue(value, exp))
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
          getEnv.flatMap(env => inject(lookup(id, env)))
        private def evalClosure(lam: SchemeLambdaExp): EvalM[Value] =
          for env <- getEnv yield newClosure(lam, env)
        protected def evalSequence(exps: List[SchemeExp]): EvalM[Value] =
          exps.foldLeftM(lattice.void)((_, exp) => eval(exp))
        private def evalSet(id: Identifier, exp: SchemeExp): EvalM[Value] =
          for
              rhs <- eval(exp)
              env <- getEnv
              _ = assign(id, env, rhs)
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
        private def evalLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
          for
              bds <- bindings.mapM { case (id, exp) => eval(exp).map(vlu => (id, vlu)) }
              res <- withEnv(env => bind(bds, env)) {
                evalSequence(body)
              }
          yield res
        private def evalLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
          bindings match
              case Nil => evalSequence(body)
              case (id, exp) :: restBds =>
                eval(exp).flatMap { rhs =>
                  withEnv(env => bind(id, env, rhs)) {
                    evalLetStar(restBds, body)
                  }
                }
        private def evalLetRec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
          withEnv(env => bindings.foldLeft(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }) {
            for
                extEnv <- getEnv
                _ <- bindings.mapM_ { case (id, exp) =>
                  eval(exp).map(value => assign(id, extEnv, value))
                }
                res <- evalSequence(body)
            yield res
          }

        protected def evalAssert(exp: SchemeExp): EvalM[Value] =
          // Assertions are not evaluated by default
          unit(lattice.void)

        private def evalCall(
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

trait BigStepModFSemantics extends BigStepModFSemanticsT {

  /* EvalM allows for big-step computations in "monadic" style */
  case class EvalM[+X](run: Environment[Address] => Option[X]):
      def flatMap[Y](f: X => EvalM[Y]): EvalM[Y] = EvalM(env => run(env).flatMap(res => f(res).run(env)))
      def map[Y](f: X => Y): EvalM[Y] = EvalM(env => run(env).map(f))

  object EvalM extends TEvalM[EvalM]:
      def map[X, Y](m: EvalM[X])(f: X => Y): EvalM[Y] = m.map(f)
      def flatMap[X, Y](m: EvalM[X])(f: X => EvalM[Y]): EvalM[Y] = m.flatMap(f)
      def unit[X](x: X): EvalM[X] = EvalM(_ => Some(x))
      def mzero[X]: EvalM[X] = EvalM(_ => None)
      def guard(cnd: Boolean): EvalM[Unit] = if cnd then EvalM(_ => Some(())) else mzero
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

  implicit val evalM = EvalM
  override def intraAnalysis(component: Component): BigStepModFIntra

  trait BigStepModFIntra extends BigStepModFIntraT {
    // analysis entry point
    def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
      eval(fnBody).run(fnEnv).foreach(res => writeResult(res))
  }

  override def configString(): String = super.configString() + "\n  applying big-step ModF Scheme semantics"
}
