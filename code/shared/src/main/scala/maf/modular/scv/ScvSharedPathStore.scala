package maf.modular.scv

import maf.core.Identity
import maf.language.symbolic.*

/**
 * Shares the entire path store (after restoring renames of symbolic variables) across function call components.
 *
 * It is implemented by writing all the path conditions (a disjunction of conjunctions) to the global store. Then this path condition is used by the
 * caller to generate successor states.
 */
trait ScvSharedPathStore extends maf.modular.scv.BaseScvBigStepSemantics with ScvSymbolicStore.GlobalSymbolicStore:
    override def intraAnalysis(component: Component): SharedPathStoreIntra

    trait SharedPathStoreIntra extends BaseIntraScvSemantics with GlobalMapStoreIntra:
        import scvMonadInstance.*
        import maf.core.Monad.MonadSyntaxOps

        private def readPathCondition(targetCmp: Component): PathCondition =
          PathCondition(readMapAddr(targetCmp))

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathCondition)] =
            //println(s"=== intra sem $cmp ==")
            val answers = super.runIntraSemantics(initialState)
            // answers.map(_._2).foreach(answer => println(s"+++ answer: ${answer.pc}"))
            writeMapAddr(cmp, Formula.join(answers.map(_._2.formula).toList: _*))
            //println(s"answer $answers")
            answers

        override protected def afterCall(targetCmp: Component): EvalM[Unit] =
            import FormulaAux.*
            // this is a very crude approximation, we propebably don't need the entire path condition from the target
            context(targetCmp) match
                case Some(k: KPathCondition[_]) =>
                  val readPc = readPathCondition(targetCmp)
                  val revertedPc = k.changes.reverse.foldLeft(readPc)((pc, change) => pc.revert(change))

                  for
                      pc <- getPc
                      _ <- putPc(PathCondition(DNF.dnf(conj(pc.formula, revertedPc.formula))))
                  yield ()
                case _ => unit(())
