package maf.modular.scv

import maf.language.scheme._
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.util.benchmarks.Timeout
import maf.util.TaggedSet
import maf.core.Identifier

/** This trait encodes the semantics of the ContractScheme language */
trait ScvBigStepSemantics extends ScvModAnalysis with ScvBaseSemantics { outer =>
  import maf.core.Monad.MonadSyntaxOps
  import maf.core.MonadStateT.{lift, unlift}
  import evalM._

  override def intraAnalysis(component: Component): IntraScvSemantics

  /** Converts a Scheme expression to a compatible Symbolic representation */
  protected def symbolic(e: SchemeExp | Symbolic): Symbolic = e match {
    case e: SchemeExp => e
  }

  /** Generates a fresh symbolic variable */
  protected def fresh: EvalM[Symbolic] = ???

  /** Tags the given value with the given Scheme expression */
  protected def tag(e: SchemeExp | Symbolic)(v: Value): EvalM[Value] = unit(v).flatMap(result => lift(TaggedSet.tag(symbolic(e), result)))

  /** Extracts the tag along with the value from a computation returning such a tagged value */
  def extract(computation: EvalM[Value]): EvalM[(Option[Symbolic], Value)] =
    flatten(unlift(computation))

  /**
   * Looks up the symbolic representation of the given variable, and returns it if it exists. Otherwise, returns a fresh symbolic representation for
   * the variable.
   */
  protected def lookupCache(id: Identifier): EvalM[Symbolic] =
    for
        env <- getEnv
        addr <- unit(
          env.lookup(id.name).getOrElse(throw Exception("variable not found"))
        ) // exception should not happen because of lexical address pass
        value <- lookupCache(addr).flatMap(v => v.map(unit).getOrElse(fresh))
    yield value

  trait IntraScvSemantics extends IntraScvAnalysis with BigStepModFIntraT:
      override def analyzeWithTimeout(timeout: Timeout.T): Unit =
        eval(program).run(State.empty)

      override def eval(exp: SchemeExp): EvalM[Value] = exp match
          // literal Scheme values have a trivial symbolic representation -> their original expression
          case SchemeValue(value, _)      => super.eval(exp).flatMap(tag(exp))
          case SchemeVar(nam)             => evalVariable(nam)
          case SchemeIf(prd, csq, alt, _) => evalIf(prd, csq, alt)
          case _                          => super.eval(exp)

      override def evalVariable(id: Identifier): EvalM[Value] =
        // the symbolic representation of a variable is the stored symbolic representation or a fresh symbolic variable
        lookupCache(id).flatMap(sym => super.evalVariable(id).flatMap(tag(sym)))

      private def symCond(prdValWithSym: (Option[Symbolic], Value), csq: SchemeExp, alt: SchemeExp): EvalM[Value] = ???

      protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
        // the if expression is evaluated in a different way, because we use symbolic information to extend the path condition and rule out unfeasible paths
        for
            prdValWithSym <- flatten(unlift(eval(prd)))
            ifVal <- symCond(prdValWithSym, csq, alt)
        yield ifVal

}
