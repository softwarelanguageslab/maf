package maf.modular.scv

import maf.language.symbolic.Symbolic.*
import maf.language.scheme.*
import maf.core.Identifier
import maf.core.Identity
import maf.language.ContractScheme.ContractValues.*

/** When mixed in ignores all the blame on locations of a "fresh" expression */
trait ScvIgnoreFreshBlame extends BaseScvBigStepSemantics:
    protected var ignoreIdns: Set[Identity] = Set()

    override def intraAnalysis(e: Component): IntraScvIgnoreFreshBlames

    trait IntraScvIgnoreFreshBlames extends BaseIntraScvSemantics:
        override def eval(e: SchemeExp): EvalM[Value] = e match
            case SchemeFuncall(SchemeVar(Identifier("fresh", _)), _, idn) =>
                ignoreIdns = ignoreIdns + idn
                super.eval(e)

            case _ => super.eval(e)

        override def writeBlame(blame: Blame): Unit =
            if !ignoreIdns.contains(blame.blamedPosition) then super.writeBlame(blame)
