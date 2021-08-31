package maf.modular.incremental.scheme.modf

import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.scheme.modf.EvalM._
import maf.modular.scheme.modf._

trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics {

  trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra with IncrementalIntraAnalysis {
    override protected def eval(exp: SchemeExp): EvalM[Value] = exp match {
      case SchemeCodeChange(e, _, _) if version == Old =>
        registerComponent(e, component)
        eval(e) // This could also be a super call if we assume no nesting of change expressions (which could be expected).
      case SchemeCodeChange(_, e, _) if version == New =>
        registerComponent(e, component)
        eval(e) // Same than above.
      case _ =>
        registerComponent(exp, component)
        super.eval(exp)
    }
    /*
    override protected def evalAndLoop(exps: List[SchemeExp]): EvalM[Value] = exps match {
      case exp :: Nil => eval(exp)
      case exp :: rst =>
        for {
          vlu <- eval(exp)
          res <- cond(vlu,
                      evalAndLoop(rst), //.map(v => addAddresses(v, getAddresses(vlu))),
                      unit(vlu)
          ) // Make sure the annotations of all computed values are kept in the result.
        } yield res
    }

    override protected def evalOr(exps: List[SchemeExp]): EvalM[Value] = exps match {
      case Nil => unit(lattice.bool(false))
      case _   => evalOrLoop(exps)
    }

    private def evalOrLoop(exps: List[SchemeExp]): EvalM[Value] = exps match {
      case exp :: Nil =>
        eval(exp) // When all values are false, it is actually the last one that flows back as the result. So we need the annotated information.
      case exp :: rst =>
        for {
          vlu <- eval(exp)
          res <- cond(vlu, unit(vlu), evalOrLoop(rst)) //.map(v => addAddresses(v, getAddresses(vlu))))
        } yield res
    }

     */
  }
}
