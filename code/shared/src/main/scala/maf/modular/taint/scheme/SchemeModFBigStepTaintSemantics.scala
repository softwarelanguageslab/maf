package maf.modular.taint.scheme

import maf.core.IdentityMonad
import maf.language.change.CodeVersion.*
import maf.core.*
import maf.core.Position.Position
import maf.language.scheme.*
import maf.language.sexp
import maf.modular.taint.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.util.benchmarks.Timeout
import maf.modular.*
import maf.modular.scheme.modf.SchemeModFComponent.Call

/** Implements big-step semantics for a Scheme DF analysis. * */
trait SchemeModFBigStepTaintSemantics extends ModAnalysis[SchemeExp] with BigStepModFSemantics with IncrementalSchemeDomain with GlobalStoreTaint[SchemeExp]:

    override def warn(msg: String): Unit = ()

    var badFlows: Set[(Addr, Addr)] = Set()

    def taintResult(): String =
        if badFlows.isEmpty
        then "No bad flows found."
        else "Some source values may reach sinks:" + badFlows.map(f => s"${f._1} => ${f._2}").mkString("\n", "\n", "\n")

    trait SchemeModFBigStepTaintIntra extends IntraAnalysis with BigStepModFIntra with GlobalStoreTaintIntra:

        override def eval(exp: SchemeExp): TEvalM.EvalM[Value] = exp match
            case SchemeSource(name, _) =>
                // Like evalVariable, but adds a source tag.
                //val a = SrcAddr(name)
                //evalVariable(name).map(v => { write(a, v); read(a) }) // Store threading is needed for sanitizers to be able to stop the trace. TODO is it?
                evalVariable(name).map(v => lattice.addAddress(v, SrcAddr(name, context(component))))
            case SchemeSanitizer(name, _) =>
                val a = SanAddr(name, context(component))
                evalVariable(name).map(v => { writeAddr(a, v); readAddr(a) })
                //evalVariable(name).map(v => lattice.addAddress(v, SanAddr(name)))
            case SchemeSink(name, _) =>
                evalVariable(name).map(v => {traceDataFlow(lattice.getAddresses(v)); v})
            case _ => super.eval(exp)

        def traceDataFlow(addr: Set[Addr]): Unit =
            var work: Set[Addr] = addr
            var visited: Set[Addr] = Set()
            while work.nonEmpty do
                val first = work.head
                work = work - first
                if !visited.contains(first)
                then
                    visited = visited + first
                    val prev: Set[Addr] = dataFlowR(first)
                    val src = prev.filter(_.isInstanceOf[SrcAddr[_]])
                    if src.nonEmpty
                    then
                        // A harmful flow was found.
                        src.foreach(s => badFlows = badFlows + ((s, addr.head))) // Initially, only contains the address of the variable read by sink.
                    prev.foreach { p =>
                      if !p.isInstanceOf[SanAddr[_]]
                      then work += p
                    }

        /**
         * Evaluation of a conditional that handles implicit value flows.
         * @note
         *   See [Liu et al. 2010].
         */
        override protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
            for
                prdVal <- eval(prd)
                // Implicit flows go from the predicate to the branches of the conditional.
                _ = { implicitFlows = lattice.getAddresses(prdVal) :: implicitFlows }
                adr = implicitFlows.flatten.toSet
                resVal <- cond(prdVal, eval(csq), eval(alt))
                _ = { implicitFlows = implicitFlows.tail }
                // Implicit flows need to be added to the return value of the if as well, as this value depends on the predicate.
            yield lattice.addAddresses(resVal, adr)

        override protected def applyClosuresM(
                                        fun: Value,
                                        args: List[(SchemeExp, Value)],
                                        cll: Position,
                                        ctx: ContextBuilder = DefaultContextBuilder,
                                    ): M[Value] =
            val arity = args.length
            val closures = lattice.getClosures(fun)
            val explicitFlows = lattice.getAddresses(fun) // Added
            MonadJoin[M].mfoldMap(closures)((clo) =>
                (clo match {
                    case (SchemeLambda(_, prs, _, _, _), _) =>
                        if prs.length == arity then
                            val argVals = args.map(_._2)
                            for
                                context <- ctx.allocM(clo, argVals, cll, component)
                                targetCall = Call(clo, context)
                                targetCmp <- newComponentM(targetCall)
                                _ = bindArgs(targetCmp, prs, argVals)
                                _ <- ctx.beforeCall(targetCmp, prs, clo)
                                result = call(targetCmp)
                                updatedResult <- afterCall(result, targetCmp, cll)
                            yield
                                implicitFlowsCut = implicitFlowsCut + (targetCmp -> (implicitFlowsCut.getOrElse(targetCmp, Set()) ++ explicitFlows ++ implicitFlows.flatten.toSet)) // Added
                                updatedResult
                        else baseEvalM.fail(ArityError(cll, prs.length, arity))
                    case (SchemeVarArgLambda(_, prs, vararg, _, _, _), _) =>
                        if prs.length <= arity then
                            val (fixedArgs, varArgs) = args.splitAt(prs.length)
                            val fixedArgVals = fixedArgs.map(_._2)

                            for
                                varArgVal <- allocateList(varArgs)
                                context <- ctx.allocM(clo, fixedArgVals :+ varArgVal, cll, component)
                                targetCall = Call(clo, context)
                                targetCmp <- newComponentM(targetCall)
                                _ = bindArgs(targetCmp, prs, fixedArgVals)
                                _ = bindArg(targetCmp, vararg, varArgVal)
                                _ <- ctx.beforeCall(targetCmp, prs, clo)
                                result = call(targetCmp)
                                updatedResult <- afterCall(result, targetCmp, cll)
                            yield
                                implicitFlowsCut = implicitFlowsCut + (targetCmp -> (implicitFlowsCut.getOrElse(targetCmp, Set()) ++ explicitFlows ++ implicitFlows.flatten.toSet)) // Added
                                updatedResult
                        else baseEvalM.fail(VarArityError(cll, prs.length, arity))
                    case _ => Monad[M].unit(lattice.bottom)
                })
            )

    override def intraAnalysis(cmp: Component): SchemeModFBigStepTaintIntra with GlobalStoreTaintIntra

    override def configString(): String = super.configString() + "\n  applying incremental big-step ModF Scheme semantics"
