package maf.modular.scv

import maf.language.scheme.*
import maf.core.{Address, Environment}
import maf.core.Position.Position
import maf.language.symbolic.PathCondition

trait ScvContractCallSensitivity extends ScvContextSensitivity with maf.modular.scv.ScvModAnalysis:

    override def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value], contractCall: Boolean = false): ContextBuilder =
        new ContextBuilder:
            def allocM(
                clo: (SchemeLambdaExp, Environment[Address]),
                args: List[Value],
                call: Position,
                caller: Component
              ): EvalM[ComponentContext] =
                // if we are calling into a contract check, we propagate the call site information
                if contractCall then scvMonadInstance.unit(KPathCondition(PathCondition.empty, rangeContract, List(call), List(), Map(), false))
                else
                    // call-site information is contamenating:
                    // if the currenct component contains a call-site in its context, the called component contains the same context
                    val callers = context(caller) match
                        case Some(ctx: KPathCondition[_]) if ctx.callers.nonEmpty => ctx.callers
                        case _                                                    => List()
                    scvMonadInstance.unit(KPathCondition(PathCondition.empty, rangeContract, callers, List(), Map(), false))

/** Sensitivity such that functions called with different closures are different components. */
trait ScvArgumentSensitivity extends ScvContextSensitivity with maf.modular.scv.ScvModAnalysis /*with maf.modular.scv.ScvContractCallSensitivity*/:
    private def isUserDefinedApplicable(v: Value): Boolean =
        // The following are considered to be user defined and applicable:
        // - flats: usually wrap around a closure
        // - closures: are user defined by their very definition
        lattice.getClosures(v).size > 0 || lattice.getFlats(v).size > 0 || lattice.getPrimitives(v).size > 0

    override def allocCtx(
        clo: (SchemeLambdaExp, Environment[Address]),
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext =
        val applicableVlus = args.filter(isUserDefinedApplicable)
        if applicableVlus.nonEmpty then KPathCondition(PathCondition.empty, None, List(), List(), Map(), false, applicableVlus)
        else NoContext()

    override def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value], contractCall: Boolean = false): ContextBuilder =
        val outerBuildCtx = super.buildCtx
        new ContextBuilder:
            def allocM(
                clo: (SchemeLambdaExp, Environment[Address]),
                args: List[Value],
                call: Position,
                caller: Component
              ): EvalM[ComponentContext] =
                val applicableVlus = args.filter(isUserDefinedApplicable)
                if applicableVlus.nonEmpty then
                    scvMonadInstance.unit(KPathCondition(PathCondition.empty, None, List(), List(), Map(), false, applicableVlus))
                else outerBuildCtx(symArgs, rangeContract, contractCall).allocM(clo, args, call, caller)
