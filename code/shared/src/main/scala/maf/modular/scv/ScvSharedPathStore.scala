package maf.modular.scv

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

        private def readPathCondition(targetCmp: Component): Set[PathStore] =
          readMapAddr(targetCmp)

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathStore)] =
            val answers = super.runIntraSemantics(initialState)
            //writeMapAddr(cmp, answers.map(_._2).toSet)
            answers

        override protected def afterCall(targetCmp: Component): EvalM[Unit] =
          // this is a very crude approximation, we propebably don't need the entire path condition from the target
          context(targetCmp) match
              case Some(KPathCondition(_, _, _, _, changes, symArgs)) =>
                val pss = readPathCondition(targetCmp)
                val updatedPss = pss.map(_.revertChanges(changes)).toSet.toList
                // Combine path store with current path store, branch if nessary
                if updatedPss.size == 0 then unit(())
                else
                    nondets(updatedPss.map { ps =>
                      for
                          pc <- getPc
                          // We only need those path conditions that actually contain information about the arguments
                          // we passed.
                          cleanedPc = ps.pc.filter(constraint => symArgs.exists(constraint.allSubexpressions.contains(_)))
                          _ = { if ps.pc.size > 0 then println(s"filtered $cleanedPc coming from ${ps.pc} with $symArgs") }
                          _ <- putPc((cleanedPc ++ pc.toSet).toList)
                      yield ()
                    }.toSet)
              case _ => unit(())
