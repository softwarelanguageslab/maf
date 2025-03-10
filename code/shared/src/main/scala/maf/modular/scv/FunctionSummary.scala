package maf.modular.scv

import maf.language.ContractScheme.ContractValues.*
import maf.language.symbolic.*
import maf.language.symbolic.Symbolic.*
import maf.core.{Address, Monad, Position}
import maf.core.Monad.MonadSyntaxOps
import maf.language.scheme.SchemeExp
import maf.core.Monad.MonadIterableOps
import maf.util.benchmarks.Timeout
import maf.util.graph.{SCC, TopSort}
import maf.util.MAFLogger
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.{Dependency, ModGraph}
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.modular.scv.ScvCallSiteReporter

/** A dataclass that holds all the information needed for a function summary */
case class FunctionSummary[V](
    /**
     * A collection of blames reported by the function, together with the formula that must be true in order fo the blame to be emitted. The second
     * element of the pair indicates whether the formula is satisfiable or not, from the perspective of a caller.
     */
    blames: Map[Blame, (Formula, IsSat[V])],
    /** A collection of paths generated by the execution of the function, as described by its path condition. */
    paths: Set[(V, PathCondition)],
    /** A collection of addresses used for their symbolic representation in the formulas in the path */
    addresses: Map[Symbolic, Set[Address]],
    addressRev: Map[Address, Symbolic],
    /** A collection that keeps track of the symbolic variables owned by this component */
    vars: Set[Symbolic])

/**
 * A dependency on the summary of another component
 *
 * @param cmp
 *   the component the function summary is about
 */
case class SummaryReadDependency[Component](cmp: Component) extends Dependency

/** An analysis that generates function summaries for each function. */
trait FunctionSummaryAnalysis extends BaseScvBigStepSemantics with ScvIgnoreFreshBlame with ScvCallSiteReporter:
    inter =>

    var functionSummaries: Map[Component, Option[FunctionSummary[Value]]] = Map().withDefaultValue(None)

    /**
     * The analysis runs in two phases: one where all components are discovered and analyzed using ModF, and another where the collected function
     * summaries are propagated.
     */
    protected var propagationPhase: Boolean = false
    protected var propagationTriggers: Map[Component, Set[Dependency]] = Map().withDefaultValue(Set())

    def runPropagationPhase(timeout: Timeout.T): Unit =
        // trigger a re-analysis of all impacted components
        propagationTriggers.flatMap(_._2).foreach(trigger)
        // re-run the analysis
        super.run(timeout)

    abstract override def run(timeout: Timeout.T): Unit =
        time(CollectionPhase) {
            // run the first phase
            super.run(timeout)
        }
        //println("collection phase is done, running propagation phase")
        propagationPhase = true
        time(PropagationPhase) {
            runPropagationPhase(timeout)
        }

    override def intraAnalysis(component: Component): FunctionSummaryIntra

    trait FunctionSummaryIntra extends BaseIntraScvSemantics with IntraScvIgnoreFreshBlames:
        private var blames: Set[(Blame, PathCondition)] = Set()
        private var addresses: Map[Symbolic, Set[Address]] = Map().withDefaultValue(Set())
        private var addressesRev: Map[Address, Symbolic] = Map()
        private var vars: Set[Symbolic] = Set()
        private lazy val myFnArgs = fnArgs

        private def buildSummary(results: Set[(PostValue, PathCondition)]): FunctionSummary[Value] =
            val summaryBlames = blames.map { case (blame, pc) => (blame -> (pc.formula, Sat[Value](Map()))) }.toMap
            val summaryResults = results.map { case (pv, pc) => (pv.value, pc) }
            FunctionSummary(blames = summaryBlames, paths = summaryResults, addresses = addresses, vars = vars, addressRev = addressesRev)

        private def scheduleTrigger(dep: Dependency): Unit =
            propagationTriggers = propagationTriggers + (component -> (propagationTriggers(component) + dep))

        /** Stores the summary in a global store such that it is accessible from the otuer intra analyses */
        private def storeSummary(summary: FunctionSummary[Value]): Unit =
            //println(s"$component -- storing summary:\nblames: ${summary.blames}\n# paths: ${summary.paths}\naddresses: ${summary.addresses}\n\n")
            //println(s"${functionSummaries(component).map(_.blames)}, ${summary.blames}")
            // we trigger interested components (for example callers) when the summary has changed
            if !functionSummaries(component).map(_ == summary).getOrElse(false) then
                val dependency = SummaryReadDependency(component)
                if !propagationPhase then scheduleTrigger(dependency) else trigger(dependency)
            functionSummaries = functionSummaries + (component -> Some(summary))

        /** Registers the blame such that it can be collected in the function summary */
        private def collectBlame(pc: PathCondition, blame: Blame): Unit =
            if !ignoreIdns.contains(blame.blamedPosition) && !blames.map(_._1).contains(blame) then blames = blames + (blame -> pc)

        private def trackAddresses(addr: Addr, sym: Symbolic, ignoreSelf: Boolean = true): Unit =
            // A symbolic write to our own arguments is always done at the beginning of the function.
            // Since that symbolic write is unconstrained (the variable is fresh and does not have any constrained associated with it), we can simply ignore any other write to it. The only difference is that we lose precision by ignoring the other write.
            if !(myFnArgs.contains(addr) && ignoreSelf) then
                addresses = addresses + (sym -> (addresses(sym) + addr))
                addressesRev = addressesRev + (addr -> sym)

        /**
         * The maximal depth of an expression before it is removed from the path condition
         *
         * @see
         *   PathCondition.gc
         * @see
         *   clean
         */
        protected val maxDepth: Int = 5

        /**
         * Clean replaces the expressions from the mapping in the given formula, and (if necessary) alpha renames any collisions as given by the list
         * of variables in `vars`.
         *
         * Any constraints in the path condition that are not about any known variables, will be removed.
         *
         * @param pc
         *   the formula that needs to be cleaned
         * @param mapping
         *   a mapping (from, to) of expressions `from` that must be replaced by expressions `to` in `pc`
         * @param vars
         *   a list of variables used in the current component. Any clashing variables in `pc` (not included the ones in `to`) will be alpha renamed.
         */
        protected def clean(pc: Formula, mapping: List[(Symbolic, Symbolic)], vars: Set[Symbolic]): Formula =
            // alpha-rename the variables that are in `vars` but are not in the `from` set of mappings, and are also in pc
            val pcVars = pc.variables
            val varNames = vars.flatMap(Symbolic.variables)
            val toRename = pcVars.toSet.intersect(varNames -- mapping.map(_._1).flatMap(Symbolic.variables)).toList
            val highest = varNames.map(_.split('x')(1).toInt).maxOption.getOrElse(0)
            val changes =
                toRename.sortBy(_.split('x')(1).toInt).zip(highest to toRename.size + highest).map { case (old, nww) =>
                    VarId(old) -> VarId(s"x$nww")
                }
            val alphaRenamedPc = pc.replace(changes.toMap)

            // first we rename the expressions in the path condition such that they correspond to `mapping`
            val mappedPc = alphaRenamedPc.replace(mapping.toMap)
            // then only keep those constraints in the path condition that contain the `vars`
            PathCondition(mappedPc).gc(vars, maxDepth).formula

        /**
         * Computes whether the sourceComponent should be composed with the targetComponent
         *
         * @param sourceComponent
         *   the component in which the targetCompopnent's paths and blames should be composed in
         * @param targetComponent
         *   the target component from which the blames and paths should be injected
         * @return
         *   true if the summary of the source should be composed with the summary of the target
         */
        protected def shouldCompose(sourceComponent: Component, targetComponent: Component): Boolean = true

        protected var totalPaths: Int = 0

        /**
         * Compose the current function summary with the received function summary
         *
         * @param vlu
         *   the value returned by the target component
         * @param targetCmp
         *   the component the summary originated from
         * @param summary
         *   the function summary itself
         */
        protected def composeWith(vlu: Value, targetCmp: Component, summary: FunctionSummary[Value]): EvalM[Value] =
            import FormulaAux.*
            // find a mapping between the arguments of the called function and our symbolic variables
            val args = fnArgs(targetCmp)
            val mapping = args.flatMap((adr) => addressesRev.get(adr).flatMap(s => summary.addressRev.get(adr).map(s2 => s2 -> s))).toList
            val doesCompose = shouldCompose(component, targetCmp)
            if summary.paths.size >= 1 && false then
                println(s"integrating ${summary.paths.size} # paths from ${targetCmp}, accumulated $totalPaths paths in total")
            for
                // propagate blames (if necessary)
                _ <- summary.blames
                    .mapM { case (blame, (pc, _)) =>
                        val cleaned = clean(pc, mapping, vars)
                        for
                            originalPc <- getPc
                            formula = conj(originalPc.formula, cleaned)
                            vars = formula.variables
                            isFeasible <- scvMonadInstance.unit(sat.feasible(formula, vars))
                            // if the blame is still feasible, we must propagate it.
                            result <-
                                if isFeasible then effectful { collectBlame(PathCondition(cleaned), blame) }
                                else scvMonadInstance.unit(())
                        yield result
                    }

                originalPc <- getPc
                // propagate (cleaned) paths, but not if we called ourselves because the paths will be the same
                value <-
                    if !doesCompose || (summary.paths.size == 0 || targetCmp == component) then scvMonadInstance.unit(vlu)
                    else
                        nondets(
                          summary.paths
                              .map { case (vlu, pc) =>
                                  val cleaned = clean(pc.formula, mapping, vars)
                                  for
                                      pc <- getPc
                                      formula = conj(pc.formula, cleaned)
                                      vars = formula.variables
                                      // check if the updated PC is still feasible, if not we can already rule it out
                                      isFeasible <- scvMonadInstance.unit(sat.feasible(formula, vars))
                                      // update if feasible
                                      _ <-
                                          if isFeasible then putPc(PathCondition(conj(pc.formula, cleaned)))
                                          else void
                                  yield vlu
                              }
                        )
                _ <- effectful { totalPaths = totalPaths + 1 }
            yield value

        /** Implements an action for the given dependency */
        override def doWrite(dep: Dependency): Boolean = dep match
            case SummaryReadDependency(_) => true
            case _                        => super.doWrite(dep)

        /** Stores a function summary in a global map after analyzing the current component */
        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathCondition)] =
            //println(s"start $component")
            // before running the intra semantics, already collect results in the temporary summary from a potential previous summary
            functionSummaries(component) match
                case Some(previousSummary) =>
                    blames = previousSummary.blames.map { case (k, (v, _)) => (k, PathCondition(v)) }.toSet
                    addresses = previousSummary.addresses
                    addressesRev = previousSummary.addressRev
                    vars = previousSummary.vars
                case _ => () // no previous summary, nothing needs to happen

            val results = super.runIntraSemantics(initialState)
            //println(s"results $results")
            val summary = buildSummary(results)
            storeSummary(summary)
            results

        /** Inject fresh symbolic variables for each variable in the list of arguments (if any) */
        override def injectCtx: EvalM[Unit] =
            for
                // ASSUMPTION: the counter of fresh variables is 0
                // required for termination.
                st <- scvMonadInstance.get
                _ = { assert(st.freshVar == 0) }
                // Map all function arguments to fresh variables
                argsCache <- fnArgs.toList.mapM(addr => fresh.map(e => (addr -> e)))
                _ <- Monad.sequence(argsCache.map { case (addr, e) =>
                    effectful { trackAddresses(addr, e, ignoreSelf = false) }
                })
                cache <- getStoreCache
                _ <- putStoreCache(cache ++ argsCache)
            yield ()

        override def writeSymbolic(addr: Addr)(e: Symbolic): EvalM[Symbolic] =
            // Keep track for all symbolic expressions on which addresses they are written.
            effectful { trackAddresses(addr, e) } >>>
                super.writeSymbolic(addr)(e)
        override def writeAddr(addr: Addr, vlu: Value): Boolean =
            if (lattice.getRight(vlu).size >= 2) then println(s"writing $vlu to $addr")
            // writeAddr can also write a symbolic value to a particular address (if getRight != Set()).
            lattice.getRight(vlu).foreach(e => trackAddresses(addr, e))
            super.writeAddr(addr, vlu)

        override def fresh: EvalM[Symbolic] =
            super.fresh.flatMap(e => effectful { vars = vars + e } >>> scvMonadInstance.unit(e))

        /**
         * This specialisation of doBlame does not write the blame directly to the global store, but instead delays that effect by writing it to the
         * function summary, to be decided by the caller of this function component
         */
        override protected def doBlame[T](blame: Blame): EvalM[T] =
            getPc.flatMap(pc => effectful { collectBlame(pc, blame) } >>> void)

        /** AFter calling a function we need to get its function summary and compose it with our current summary */
        override def afterCall(vlu: Value, targetCmp: Component, cll: Position.Position): EvalM[Value] =
            // track the number of distinct call sites to a particular component
            trackCallSite(targetCmp, cll)
            // we are interested in a change to the summary of the target component
            register(SummaryReadDependency(targetCmp))
            if functionSummaries(targetCmp).isDefined && propagationPhase then composeWith(vlu, targetCmp, functionSummaries(targetCmp).get)
            else super.afterCall(vlu, targetCmp, cll)

trait FunctionSummaryAnalysisWithMainBoundary extends FunctionSummaryAnalysis with StandardSchemeModFComponents:

    abstract override def run(timeout: Timeout.T): Unit =
        // run the analysis until completion
        super.run(timeout)
        // if there are still blames at the main component, then register them as actual blames
        val mainSummary = functionSummaries(Main)
        mainSummary.map(_.blames.foreach { (blame, _) =>
            writeAddr(ScvExceptionAddr(Main, program.idn), lattice.blame(blame))
        })

/** This trait provides a `buildCollapsedGraph` method that collapses the function summary graph into a graph that does not contain any cycles */
trait SccGraph extends FunctionSummaryAnalysis with ModGraph[SchemeExp]:
    sealed trait Node
    object Node:
        def fromDep(source: Dependency): Option[Node] = source match
            case d: SummaryReadDependency[_] => Some(DepNode(d))
            case _                           => None

        def fromCmp(source: Component): Option[Node] = Some(ComponentNode(source))

    case class ComponentNode(c: Component) extends Node
    case class DepNode(dep: Dependency) extends Node

    def buildCollapsedGraph: (Set[Set[Node]], Map[Set[Node], Set[Set[Node]]]) =
        val transformedRead = deps.flatMap { case (dep, cmps) =>
            Node.fromDep(dep).map(n => (n -> cmps.flatMap(Node.fromCmp)))
        }
        val transformedWrite = triggeredDeps.flatMap { case (cmp, deps) =>
            Node.fromCmp(cmp).map(n => (n -> deps.flatMap(Node.fromDep)))
        }
        val completeGraph = transformedRead.foldLeft(transformedWrite) { case (deps, (from, toS)) =>
            deps + (from -> (deps.get(from).getOrElse(Set()) ++ toS))
        }
        val nodes = completeGraph.keySet

        SCC.collapse(nodes, completeGraph)

    override def intraAnalysis(component: Component): SccGraphIntra

    trait SccGraphIntra extends FunctionSummaryIntra with IntraTrackAnalysis

/**
 * Instead of relying on an arbitrary method of scheduling the to be analyzed components, we can also topologically sort them based on the call graph
 * of the function summaries
 */
trait TopSortPropagationPhase extends SccGraph with FunctionSummaryAnalysis:
    private var isFinished: Boolean = false

    abstract override def finished: Boolean =
        if propagationPhase then isFinished else super.finished

    private def runAnalysis(timeout: Timeout.T)(cmp: Component): Boolean =
        if timeout.reached then false
        else
            val intra = intraAnalysis(cmp)
            intra.analyzeWithTimeout(timeout)
            intra.commit()
            true

    override def runPropagationPhase(timeout: Timeout.T): Unit =
        //println("running propagation phase")
        // add the scheduled triggers to the actual triggers
        triggeredDeps = propagationTriggers.foldLeft(triggeredDeps) { case (triggeredDeps, (cmp, dep)) =>
            triggeredDeps + (cmp -> (triggeredDeps.get(cmp).getOrElse(Set()) ++ dep))
        }

        // collapse the cycles in the graph
        val (nodes, edges) = buildCollapsedGraph
        val schedule = TopSort
            .topsort(nodes.toList, edges)
            .map(_.collect { case ComponentNode(cmp) =>
                cmp
            })

        // run the analysis on all the clusters (in order)
        val unanalyzed = schedule.filterNot(cluster => cluster.forall(runAnalysis(timeout)))
        isFinished = unanalyzed.size == 0

        // add the unanalyzed components to the worklist such that anl.isfinished returns false
        unanalyzed.flatten.foreach(addToWorkList)

trait NoCompositionIfCycle extends FunctionSummaryAnalysis with SccGraph:
    override def intraAnalysis(component: Component): NoCompositionIfCycleIntra

    trait NoCompositionIfCycleIntra extends FunctionSummaryIntra with SccGraphIntra:
        override protected val maxDepth: Int = Int.MaxValue

        override protected def shouldCompose(sourceComponent: Component, targetComponent: Component): Boolean =
            val (scc, _) = buildCollapsedGraph
            !scc.exists(cluster => cluster.contains(ComponentNode(sourceComponent)) && cluster.contains(ComponentNode(targetComponent)))

trait CompositionForContracts extends FunctionSummaryAnalysis:
    override def intraAnalysis(cmp: Component): CompositionForContractsIntra

    trait CompositionForContractsIntra extends FunctionSummaryIntra:
        override protected def shouldCompose(sourceComponent: Component, targetComponent: Component): Boolean =
            (context(targetComponent) match
                case Some(k: KPathCondition[_]) if k.callers.nonEmpty => true
                case _                                                => false
            ) && super.shouldCompose(sourceComponent, targetComponent)
