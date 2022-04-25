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

/** A full path sensitive analysis that shares path conditions from callees with callers */
trait ScvFullPathSensitivity extends BaseScvBigStepSemantics with ScvPathSensitiveSymbolicStore.GlobalPathSensitiveSymbolicStore:
    override def intraAnalysis(component: Component): ScvFullPathSensitivityIntra

    trait ScvFullPathSensitivityIntra extends BaseIntraScvSemantics with GlobalMapStoreIntra:
        import scvMonadInstance.*

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathCondition)] =
            //println(s"analyzing $cmp")
            //println(s"number of components ${trackMetrics(NumberOfComponents).size}")
            val answers: Set[(PostValue, PathCondition)] = super.runIntraSemantics(initialState)
            answers.foreach { case (PostValue(sym, vlu), pc) =>
                //println(s"++ got value $vlu with $pc and $sym")
                writeMapAddr(cmp, Map(pc.formula -> lattice.setRight(vlu, Set())))
            }
            answers

        override protected def afterCall(vlu: Value, targetCmp: Component): EvalM[Value] =
            import FormulaAux.*

            context(targetCmp) match
                case Some(k: KPathCondition[_]) if readMapAddr(targetCmp).size > 0 =>
                    // Construct a successor state for all the paths originating from the callee
                    val paths = readMapAddr(targetCmp).map { case (formula, (vlu)) =>
                        val syms = lattice.getRight(vlu)
                        //println(s"== formula: $formula and vlu: $vlu with $syms, ${k.changes}")
                        val pc = PathCondition(formula)
                        val gcPc = pc.gc(k.args)
                        //println(s"== formula_gc: pc $gcPc")
                        val revertedPc = k.changes.reverse.foldLeft(gcPc)((pc, change) => pc.revert(change))

                        for
                            oldPc <- getPc
                            _ <- putPc(PathCondition(conj(oldPc.formula, revertedPc.formula)))
                            resVlu <- if syms.size > 0 then nondets(syms.map(tag(_)(vlu))) else unit(vlu)
                        yield resVlu
                    }

                    nondets(paths.toSet)

                case _ => unit(vlu)
