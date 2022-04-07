package maf.modular.scv

import maf.core.Identity
import maf.language.symbolic.*
import maf.language.scheme.SchemeExp
import maf.core.Monad.MonadSyntaxOps
import maf.core.Address

case class PcAddr(pc: PathCondition) extends Address:
    def idn: Identity = Identity.none
    def printable: Boolean = false
    override def toString: String = s"pc($pc)"

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

        private def readPathCondition(targetCmp: Component): PathCondition =
            PathCondition(readMapAddr(targetCmp))

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathCondition)] =
            val answers = super.runIntraSemantics(initialState)
            val formulas = answers.map(_._2.formula).toList
            val values = answers.map(_._1._2).toList
            writeMapAddr(cmp, Formula.join(formulas: _*))
            answers

        override protected def afterCall(vlu: Value, targetCmp: Component): EvalM[Value] =
            import FormulaAux.*
            // this is a very crude approximation, we propebably don't need the entire path condition from the target
            context(targetCmp) match
                case Some(k: KPathCondition[_]) =>
                    val readPc = readPathCondition(targetCmp)
                    val gcPc = readPc.gc(k.symArgs.values.toSet)

                    val revertedPc = k.changes.reverse.foldLeft(gcPc)((pc, change) => pc.revert(change))

                    for
                        pc <- getPc
                        _ <- putPc(PathCondition(DNF.dnf(conj(pc.formula, revertedPc.formula))))
                    yield vlu
                case _ => unit(vlu)

/** A full path sensitive analysis that shares path conditions from callees with callers */
trait ScvFullPathSensitivity extends BaseScvBigStepSemantics with ScvPathSensitiveSymbolicStore.GlobalPathSensitiveSymbolicStore:
    override def intraAnalysis(component: Component): ScvFullPathSensitivityIntra

    trait ScvFullPathSensitivityIntra extends BaseIntraScvSemantics with GlobalMapStoreIntra:
        import scvMonadInstance.*

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathCondition)] =
            val answers: Set[(PostValue, PathCondition)] = super.runIntraSemantics(initialState)
            answers.foreach { case (PostValue(sym, vlu), pc) =>
                //println(s"++ got value $vlu with $pc and $sym")
                writeMapAddr(cmp, Map(pc.formula -> lattice.setRight(vlu, Set())))
            }
            answers

        override protected def afterCall(vlu: Value, targetCmp: Component): EvalM[Value] =
            import FormulaAux.*

            context(targetCmp) match
                case Some(k: KPathCondition[_]) =>
                    // Construct a successor state for all the paths originating from the callee
                    val paths = readMapAddr(targetCmp).map { case (formula, (vlu)) =>
                        val syms = lattice.getRight(vlu)
                        //println(s"== formula: $formula and vlu: $vlu with $syms")
                        val pc = PathCondition(formula)
                        val gcPc = pc.gc(k.symArgs.values.toSet)
                        val revertedPc = k.changes.reverse.foldLeft(gcPc)((pc, change) => pc.revert(change))

                        for
                            oldPc <- getPc
                            _ <- putPc(PathCondition(conj(oldPc.formula, revertedPc.formula)))
                            resVlu <- if syms.size > 0 then nondets(syms.map(tag(_)(vlu))) else unit(vlu)
                        yield resVlu
                    }

                    nondets(paths.toSet)

                case _ => unit(vlu)
