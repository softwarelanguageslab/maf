package maf.modular.adaptive.scheme

import maf.core._
import maf.language.scheme._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.modular.scheme._
import maf.modular._
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.datastructures.MultiSet
import maf.util.benchmarks.Table
import maf.util._

import scala.util.Random

import com.cibo.evilplot._
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.numeric.Point

import java.io.File

trait AdaptiveContextSensitivity(b: Int = 0) extends AdaptiveSchemeModFSemantics:
    this: AdaptiveContextSensitivityPolicy =>

    import modularLattice.Elements.*

    val strategy: String
    val visualise = true

    var inspectCount = 0

    // disable warning messages and debug logging by default (can be override for custom logging)
    protected def warn(message: => String): Unit = ()
    protected def debug(message: => String): Unit = ()

    // extra parameters to control the "aggressiveness" of the adaptation (TODO: in Scala 3, make these (default?) trait parameters):
    // - `reduceFactor`: determines by what factor the number of components needs to be reduced
    // - `cutoffFactor`: determines the cutoff for selecting "culprits" to be reduced in the adaptation
    val reduceFactor = 0.5
    val cutoffFactor = 0.5
    val budget = b

    // use a different context-sensitivity policy per closure

    protected var policyPerFn: Map[LambdaModule, ContextSensitivityPolicy] = Map.empty
    protected def getCurrentPolicy(fn: LambdaModule): ContextSensitivityPolicy =
        policyPerFn.getOrElse(fn, defaultPolicy)
    protected def setCurrentPolicy(fn: LambdaModule, ply: ContextSensitivityPolicy): Unit =
        policyPerFn += fn -> ply

    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext =
        getCurrentPolicy(LambdaModule(clo._1)).allocCtx(clo, args, call, caller)
    def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] = cll match
        case Call(clo, ctx) => Call(adaptClosure(clo), adaptCtx(LambdaModule(clo._1), ctx))
    def adaptCtx(fn: LambdaModule, ctx: ComponentContext): ComponentContext =
        getCurrentPolicy(fn).adaptCtx(ctx)

    // allocation context = component context
    // (also store the function that the context came from)

    type AllocationContext = Option[(ComponentContext, LambdaModule)]
    def adaptAllocCtx(ctx: AllocationContext): AllocationContext = ctx.map { case (ctx, idn) =>
        (getCurrentPolicy(idn).adaptCtx(ctx), idn)
    }
    private def addrContext(cmp: SchemeModFComponent) = cmp match
        case Main                                             => None
        case Call((lam, _), ctx: ComponentContext @unchecked) => Some((ctx, LambdaModule(lam)))
    def allocPtr(exp: SchemeExp, cmp: SchemeModFComponent) = PtrAddr(exp, addrContext(cmp))
    def allocVar(idf: Identifier, cmp: SchemeModFComponent) = VarAddr(idf, addrContext(cmp))

    // during the analysis, keep track of
    // - per module: all components
    // - per module: how many times each component has been triggered
    // - dependencies (per component) that triggered a component
    // - the number of times a dependency has been triggered

    protected var allCmpsPerFn: Map[LambdaModule, Set[Call[ComponentContext]]] = Map.empty
    protected var cmpsPerFn: Map[SchemeModule, MultiSet[Component]] = Map.empty
    protected var depsPerCmp: Map[Component, Set[Dependency]] = Map.empty
    protected var depCounts: Map[Dependency, Int] = Map.empty

    override def spawn(cmp: Component) =
        if !visited(cmp) then
            val mod = module(cmp).asInstanceOf[LambdaModule]
            val call = cmp.asInstanceOf[Call[ComponentContext]]
            cmpsPerFn += mod -> (cmpsPerFn.getOrElse(mod, MultiSet.empty) + cmp)
            allCmpsPerFn += mod -> (allCmpsPerFn.getOrElse(mod, Set.empty) + call)
        super.spawn(cmp)

    override def trigger(dep: Dependency) =
        deps.getOrElse(dep, Set.empty).foreach { cmp =>
            val mod = module(cmp)
            cmpsPerFn += mod -> (cmpsPerFn.getOrElse(mod, MultiSet.empty) + cmp)
            depsPerCmp += cmp -> (depsPerCmp.getOrElse(cmp, Set.empty) + dep)
        }
        depCounts += dep -> (depCounts.getOrElse(dep, 0) + 1)
        super.trigger(dep)

    // ... during adaptation (to avoid duplicating work), keep track of:
    // - modules that have been reduced
    // - dependencies that have been reduced

    protected var reducedModules: Set[SchemeModule] = Set.empty
    protected var reducedDeps: Set[Dependency] = Set.empty

    // data collection
    protected var chosenModules: Table[Int] = Table.empty.withDefaultValue(0)

    // adapting the analysis
    def inspect() =
        if tooManyIntraanalyses(cmpsPerFn) then {
            inspectCount = inspectCount + 1
            val adaptationCandidates = cmpsPerFn.filter({case (m: LambdaModule, _) => !getCurrentPolicy(m).isLowestPolicy
                                                         case (_, _)               => true})
            def modulesToAdapt = selectStartingModule(adaptationCandidates)
            // start the adaptation
            modulesToAdapt.foreach { case (module, _) =>
                chosenModules = chosenModules.add(module.toString(), inspectCount.toString(), 1)
                reduceModule(module)
            }
            // update the analysis
            if reducedModules.nonEmpty then { adaptAnalysis() }
            // data collection: save the modules that have been adapted
            if visualise then writeToFile(chosenModules.toCSVString(rows = chosenModules.allRows.toList), s"out/adaptive-viz/chosenModules/$strategy.csv")
            // clear the set of reduced modules
            reducedModules = Set.empty
            reducedDeps = Set.empty
        }

    // selecting a starting module + when to start adapting
    protected def tooManyIntraanalyses(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]): Boolean
    protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]): Iterable[(SchemeModule, MultiSet[Component])]


    // REDUCING
    private def reduceModule(module: SchemeModule): Unit =
        val moduleCmps = cmpsPerFn(module)
        val numberOfComponents = moduleCmps.distinctCount
        val maximumComponentCost = moduleCmps.content.maxBy(_._2)._2
        val judgement = numberOfComponents > maximumComponentCost 

        module match {
            case m: LambdaModule => if visualise then visualiseComponentsForModule(m, judgement)}

        if judgement then 
            module match {
                case m : LambdaModule => reduceComponentsForModule(m)
                case _                => return
            }
        else
            val adaptationCandidates = moduleCmps.filter({case m: LambdaModule => !getCurrentPolicy(m).isLowestPolicy
                                                          case _               => true})
            val selectedCmps = selectLargest[(Component, Int)](moduleCmps.content, _._2)
            selectedCmps.foreach { case (cmp, _) => reduceReanalysesForComponent(cmp) }

    private def reduceReanalysesForComponent(cmp: Component) =
        val deps = depsPerCmp.getOrElse(cmp, Set.empty)
        val groupedByLoc = deps.groupBy(getDepExp)
        val adaptationCandidates = groupedByLoc.filter((_, deps) => deps.size > 1)
        val selected = selectLargest[(Expression, Set[Dependency])](groupedByLoc, (_: Expression, deps: Set[Dependency]) => deps.size)
        selected.foreach { case (loc, deps) => reduceTriggersForLocation(loc, deps) }

    private def reduceTriggersForLocation(loc: Expression, deps: Set[Dependency]) =
        val numberOfDependencies = deps.size
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        
        if numberOfDependencies > maximumDependencyCost then reduceAddressesForLocation(loc, deps.map(_.asInstanceOf[AddrDependency].addr))
        else
            val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
            val selected = selectLargest[Dependency](deps, depCounts, maximumDependencyCost)
            selected.foreach { dep => reduceDep(dep) }

    private def reduceComponentsForModule(module: LambdaModule): Unit =
        val calls = allCmpsPerFn(module)
        val groupedByClo = calls.groupBy(_.clo) 
        val cloMaxContexts = groupedByClo.maxBy(_._2.size)._2.size 
        if cloMaxContexts > groupedByClo.size then reduceContextsForModule(module)
        else getParentModule(calls.head.clo) match {
            case m : LambdaModule => reduceComponentsForModule(m)
            case MainModule       => return
        }

    // VISUALISATIONS

    var moduleChart: Map[LambdaModule, Seq[Plot]] = Map.empty
    var largestBoundsModuleChart: Map[LambdaModule, (Int, Int)] = Map.empty
        
    private def visualiseComponentsForModule(module: LambdaModule, judgement: Boolean): Unit = 
        val calls = allCmpsPerFn(module)
        val groupedByClo = calls.groupBy(_.clo) 
        val cloMaxContexts = groupedByClo.maxBy(_._2.size)._2.size

        val prevBounds = largestBoundsModuleChart.getOrElse(module, (1,1))
        val currentBounds = (Math.max(prevBounds._1, groupedByClo.size), Math.max(prevBounds._2, cloMaxContexts))
        largestBoundsModuleChart = largestBoundsModuleChart + (module -> currentBounds)
        val plot = BarChart(groupedByClo.toSeq.map(_._2.size))
                        .title(s"$inspectCount ${judgement.toString}")
                        .xAxis()
                        .yAxis()
                        .frame()
        moduleChart = moduleChart + (module -> (moduleChart.getOrElse(module, Seq.empty) ++ Seq(plot)))
        // ensure every chart for this module has the same bounds
        moduleChart = moduleChart + (module -> (moduleChart.getOrElse(module, Seq.empty).map(_.xbounds(0, currentBounds._1).ybounds(0, currentBounds._2))))
        val facets = Facets(moduleChart.get(module).toSeq).title(module.toString).xLabel("closures").yLabel("contexts")
        val file = new File(s"out/adaptive-viz/componentsForModule/${strategy}/${module.toString.replace(" ", "_")}.png")
        file.mkdirs()
        facets.render().write(file)
        // todo: show this also after the adaptation to see the difference the adaptation makes

    // find a fitting policy
    protected def reduceContextsForModule(module: LambdaModule): Unit =
        if !reducedModules(module) then // ensure this is only done once per module per adaptation
            reducedModules += module
            // find a fitting CS policy
            var ctxs = allCmpsPerFn(module).map(_.ctx)
            var plcy = getCurrentPolicy(module)
            val target = Math.max(1, ctxs.size * reduceFactor) 

            while ctxs.size > target do
                // need to decrease precision further
                plcy = nextPolicy(module.lambda, plcy, ctxs)
                ctxs = ctxs.map(plcy.adaptCtx)
            // register the new policy
            debug(s"$module -> $plcy")
            setCurrentPolicy(module, plcy)

    private def reduceDep(dep: Dependency) =
        if !reducedDeps(dep) then
            reducedDeps += dep
            dep match
                case AddrDependency(addr) => reduceValueAbs(store(addr))
                case _                    => throw new Exception("Unknown dependency for adaptive analysis")

    private def reduceValueAbs(value: Value): Unit = value.vs.maxBy(sizeOfV) match
        case modularLatticeWrapper.modularLattice.Pointer(pts) => reduceAddresses(pts)
        case modularLatticeWrapper.modularLattice.Clo(cls)     => reduceClosures(cls)
        case modularLatticeWrapper.modularLattice.Cons(car, cdr) =>
            if sizeOfValue(car) > sizeOfValue(cdr) then reduceValueAbs(car)
            else reduceValueAbs(cdr)
        case modularLatticeWrapper.modularLattice.Vec(_, elms) =>
            val value = elms.map(_._2).maxBy(sizeOfValue) // assume elms is not empty!
            reduceValueAbs(value)
        case v => warn(s"Attempting to adapt a non-set-based value $v")

    private def reduceAddresses(addrs: Set[Addr]) =
        val groupByLocation = addrs.groupBy[Expression](getAddrExp)
        val selected = selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)
        selected.foreach { case (loc, addrs) => reduceAddressesForLocation(loc, addrs) }

    private def reduceAddressesForLocation(loc: Expression, addrs: Set[Addr]): Unit =
        //debug(s"Reducing ${addrs.size} addrs")
        getAddrModule(addrs.head) match {
            case m : LambdaModule => reduceContextsForModule(m)
            case _                => return
        }

    private def reduceClosures(cls: Set[(SchemeLambdaExp, Environment[Addr])]) =
        val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
        val selected = selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)
        selected.foreach { case (fn, closures) => reduceClosuresForFunction(fn, closures) }

    private def reduceClosuresForFunction(fn: SchemeLambdaExp, closures: Set[(SchemeLambdaExp, Environment[Addr])]): Unit =
        //debug(s"Reducing ${closures.size} closures")
        getParentModule(closures.head) match {
            case m : LambdaModule => reduceContextsForModule(m)
            case _                => return
        }

    override def adaptAnalysis() =
        super.adaptAnalysis()
        this.allCmpsPerFn = adaptMap(adaptSet(adaptCall))(allCmpsPerFn)
        this.cmpsPerFn = Map.empty
        this.depsPerCmp = Map.empty
        this.depCounts = Map.empty

    /*
     * HELPERS
     * (TODO: factor our some of these ...)
     */

    def writeToFile(output: String, path: String) = 
      val writer = Writer.open(path)
      Writer.write(writer, output)
      Writer.close(writer)

    def selectLargest[D](data: Iterable[D], size: D => Int): Iterable[D] =
        if !data.isEmpty then
            selectLargest(data, size, size(data.maxBy(size)))
        else data
    def selectLargest[D](
        data: Iterable[D],
        size: D => Int,
        max: Int
      ): Iterable[D] =
        val target = Math.max(max * cutoffFactor, 1)
        data.filter(size(_) > target)

    private def getDepExp(dep: Dependency): Expression = dep match
        case AddrDependency(addr) => getAddrExp(addr)
        case _                    => throw new Exception(s"Unknown dependency: $dep")
    private def getAddrExp(addr: Addr): Expression = addr match
        case returnAddr: ReturnAddr[Component] @unchecked => expr(returnAddr.cmp)
        case schemeAddr: SchemeAddr[AllocationContext] @unchecked =>
            schemeAddr match
                case VarAddr(idf, _) => idf
                case PtrAddr(exp, _) => exp
                case PrmAddr(nam)    => Identifier(nam, Identity.none)
    private def getAddrModule(addr: Addr): SchemeModule = addr match
        case returnAddr: ReturnAddr[Component] @unchecked => module(returnAddr.cmp)
        case schemeAddr: SchemeAddr[AllocationContext] @unchecked =>
            schemeAddr match
                case VarAddr(_, ctx) => getAllocCtxModule(ctx)
                case PtrAddr(_, ctx) => getAllocCtxModule(ctx)
                case PrmAddr(_)      => MainModule
    private def getAllocCtxModule(ctx: AllocationContext): SchemeModule = ctx match
        case None          => MainModule
        case Some((_, lm)) => lm
    def getParentModule(clo: (SchemeLambdaExp, Environment[Addr])): SchemeModule =
        // gets the enclosing lambda
        clo._2.asInstanceOf[WrappedEnv[Addr, SchemeModule]].data
    private def sizeOfValue(value: Value): Int =
        value.vs.map(sizeOfV).sum
    private def sizeOfV(v: modularLattice.Value): Int = v match
        case modularLatticeWrapper.modularLattice.Pointer(ptrs)    => ptrs.size
        case modularLatticeWrapper.modularLattice.Clo(closures)    => closures.size
        case modularLatticeWrapper.modularLattice.Cons(car, cdr)   => sizeOfValue(car) + sizeOfValue(cdr)
        case modularLatticeWrapper.modularLattice.Vec(_, elements) => elements.map(_._2).map(sizeOfValue).sum
        case _                                                     => 0


// STRATEGIES
trait TooManyRandom extends AdaptiveContextSensitivity: 
    // randomly choose to adapt
    this: AdaptiveContextSensitivityPolicy =>

    override protected def tooManyIntraanalyses(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]): Boolean = 
        Random.nextBoolean()

trait TooManyAlways extends AdaptiveContextSensitivity: 
    // always choose to adapt
    this: AdaptiveContextSensitivityPolicy =>

    override protected def tooManyIntraanalyses(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]): Boolean = 
        true

trait TooManyCost extends AdaptiveContextSensitivity:
    // choose when to adapt based on a maximum cost
    this: AdaptiveContextSensitivityPolicy => 


    override protected def tooManyIntraanalyses(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]): Boolean = 
        !cmpsPerFn.filter({(_: SchemeModule, cmps: MultiSet[Component]) => cmps.cardinality > budget}).isEmpty  

// adaptation target
trait SelectRandom extends AdaptiveContextSensitivity: 
    // select randomly
    this: AdaptiveContextSensitivityPolicy =>

    val strategy: String = "select-random"
    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        Random.shuffle(cmpsPerFn.toList).take(1)

trait SelectMostContexts extends AdaptiveContextSensitivity: 
    // select the components with the most different contexts
    this: AdaptiveContextSensitivityPolicy =>
    val strategy: String = "select-mostcontexts"

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) =
        // the module with the most components 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, _._2.distinctCount) 

trait SelectBudget extends AdaptiveContextSensitivity: 
    // select the components that go over the given budget
    this: AdaptiveContextSensitivityPolicy =>

    val strategy: String = s"select-budget-$budget"

    // select the modules with more reanalyses than allowed by the budget 
    // (combined reanalyses for every component of the module)
    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, _._2.cardinality, budget)

trait SelectMostDependencies extends AdaptiveContextSensitivity: 
    // select the components with the most dependencies
    this: AdaptiveContextSensitivityPolicy => 

    val strategy: String = "select-mostdependencies"

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 depsPerCmp.getOrElse(cmp, Set.empty).size + x} ))

trait SelectLeastDependencies extends AdaptiveContextSensitivity: 
    // select the components with the least dependencies
    this: AdaptiveContextSensitivityPolicy => 

    val strategy: String = "select-leastdependencies"

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 - (depsPerCmp.getOrElse(cmp, Set.empty).size + x)} ))
 
trait SelectImprecise extends AdaptiveContextSensitivity: 
    // select the components with the most imprecise values (ie highest in the lattice)
    this: AdaptiveContextSensitivityPolicy => 

    val strategy: String = "select-imprecise"

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 x + lattice.elementSize(returnValue(cmp))} ))

trait SelectDifferentValues extends AdaptiveContextSensitivity: 
    this: AdaptiveContextSensitivityPolicy => 

    val strategy: String = "select-differentvalues"

    // select modules where the contexts do not represent many different abstract values
    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                            Math.round((MultiSet(cmps.content.map({ (cmp: Component, x: Int) => (returnValue(cmp), x)}), 
                                                                     cmps.cardinality).distinctCount / cmps.cardinality) * 10)
                                                              ))
