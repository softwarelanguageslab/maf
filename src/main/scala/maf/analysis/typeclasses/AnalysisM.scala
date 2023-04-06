package maf.analysis
package typeclasses

import maf.util.*
import maf.analysis.store.*
import maf.syntax.sexp.*
import maf.syntax.scheme.*
import cats.syntax.all.*
import cats.Monad
import maf.values.scheme.*
import maf.syntax.*
import primitives.*
import maf.values.typeclasses.Galois.*
import maf.interpreter.*
import maf.interpreter.ConcreteSchemeValue.given
import maf.values.scheme.given
import maf.values.typeclasses.Galois.inject

/** Operations for interacting with the environment.
  *
  * Environments are nested and lexical meaning that a typical `MonadReader` should be sufficient for this
  */
trait EnvironmentM[M[_]: Monad, Val]:
    type Env = Environment[ValAddress[Val]]

    def getEnv: M[Env]
    def withEnv[X](f: Env => Env)(blk: M[X]): M[X]
    def withEnvM[X](f: Env => M[Env])(blk: M[X]): M[X] =
        for
            oldEnv <- getEnv
            newEnv <- f(oldEnv)
            res <- withEnv(_ => newEnv) { blk }
        yield res

    def lookupEnv(id: Var): M[ValAddress[Val]] =
        getEnv map (_.lookup(id.name).getOrElse(
          throw new Exception(s"undefined variable ${id.name}@${id.idn}")
        ))
    def withExtendedEnv[X](nam: String, adr: ValAddress[Val])(blk: M[X]): M[X] =
        withEnv(_.extend(nam, adr))(blk)
    def withExtendedEnv[X](bds: Iterable[(String, ValAddress[Val])])(blk: M[X]): M[X] =
        withEnv(_.extend(bds))(blk)

trait CtxM[M[_]: Monad]:
    type Ctx

    def getCtx: M[Ctx]
    def withCtx[X](ctx: Ctx => Ctx)(blk: M[X]): M[X]

trait CallM[M[_], Val]:
    type Lam = SchemeLambdaExp
    type Env = Environment[ValAddress[Val]]

    def call(lam: Lam): M[Val]

trait EvalM[M[_], Val]:
    def eval(e: SchemeExp): M[Val]

trait SchemeContextSensitivityM[M[_], Val, Vec, Pai] extends AnalysisM[M, Val, Vec, Pai]:
    def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx): Ctx

trait AnalysisM[M[_], Val, Vec, Pai] extends SchemePrimM[M, Val, Vec, Pai], CtxM[M], CallM[M, Val]

object AnalysisM:
    def apply[M[_], Val, Vec, Pai](
        using
        anl: AnalysisM[M, Val, Vec, Pai]
      ): AnalysisM[M, Val, Vec, Pai] = anl

trait SchemeSemanticsM[M[_], V, Vec, Pai]
    extends AnalysisM[M, V, Vec, Pai]
    with SchemeContextSensitivityM[M, V, Vec, Pai]
    with EnvironmentM[M, V]
    with EvalM[M, V]

object SchemeSemanticsM:
    def apply[A[_], Val, Vec, Pai](using sem: SchemeSemanticsM[A, Val, Vec, Pai]) = sem
