package maf.modular.scv

import maf.language.symbolic.Symbolic.*
import maf.language.scheme.SchemeExp
import maf.core.Identity
import maf.language.ContractScheme.ContractValues.*

/** When mixed in ignores all the blame on locations of a "fresh" expression */
trait ScvIgnoreFreshBlame extends BaseScvBigStepSemantics:
    private var ignoreIdns: Set[Identity] = Set()

    override def intraAnalysis(e: Component): IntraScvIgnoreFreshBlames

    trait IntraScvIgnoreFreshBlames extends BaseIntraScvSemantics:
        override def eval(e: SchemeExp): EvalM[Value] = e match
            case Hole(idn) =>
                ignoreIdns = ignoreIdns + idn
                super.eval(e)

            case _ => super.eval(e)

        override def writeBlame(blame: Blame): Unit =
            if !ignoreIdns.contains(blame.blamedPosition) then super.writeBlame(blame)
