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

import scala.util.Random

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics:
    this: AdaptiveContextSensitivityPolicy =>

    import modularLattice.Elements.*

    // disable warning messages and debug logging by default (can be override for c3ustom logging)
    protected def warn(message: => String): Unit = ()
    protected def debug(message: => String): Unit = ()

    // extra parameters to control the "aggressiveness" of the adaptation (TODO: in Scala 3, make these (default?) trait parameters):
    // - `reduceFactor`: determines by what factor the number of components needs to be reduced
    // - `cutoffFactor`: determines the cutoff for selecting "culprits" to be reduced in the adaptation
    val reduceFactor = 0.5
    val cutoffFactor = 0.5

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

    // adapting the analysis
    def inspect() =
        // start the adaptation
        def modulesToAdapt = selectStartingModule(cmpsPerFn)
        modulesToAdapt.foreach { case (module, _) =>
            reduceModule(module)
        }
        // update the analysis
        if reducedModules.nonEmpty then { adaptAnalysis() }
        // clear the set of reduced modules
        reducedModules = Set.empty
        reducedDeps = Set.empty

    // WHEN TO ADAPT
    protected def tooManyComponents(moduleCmps: MultiSet[Component]): Boolean
    protected def tooManyDependencies(deps: Set[Dependency]): Boolean
    protected def tooManyContexts(calls: Set[Call[ComponentContext]]): Boolean

    // WHAT TO ADAPT
    protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]): Iterable[(SchemeModule, MultiSet[Component])]
    protected def selectComponent(moduleCmps:  MultiSet[Component]): Iterable[(Component, Int)]
    protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]): Iterable[(Expression, Set[Dependency])]
    protected def selectTrigger(deps: Set[Dependency]): Iterable[Dependency]
    protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]): Iterable[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])]
    protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]): Iterable[(Expression, Set[Addr])]

    // REDUCING
    private def reduceModule(module: SchemeModule): Unit =
        val moduleCmps = cmpsPerFn(module)
        if tooManyComponents(moduleCmps) then 
            module match {
                case m : LambdaModule => reduceComponentsForModule(m)
                case _                => return
            }
        else
            val selectedCmps = selectComponent(moduleCmps)
            selectedCmps.foreach { case (cmp, _) => reduceReanalysesForComponent(cmp) }

    private def reduceReanalysesForComponent(cmp: Component) =
        val deps = depsPerCmp.getOrElse(cmp, Set.empty)
        val groupedByLoc = deps.groupBy(getDepExp)
        val selected = selectReanalysis(groupedByLoc)
        selected.foreach { case (loc, deps) => reduceTriggersForLocation(loc, deps) }

    private def reduceTriggersForLocation(loc: Expression, deps: Set[Dependency]) =
        if tooManyDependencies(deps) then reduceAddressesForLocation(loc, deps.map(_.asInstanceOf[AddrDependency].addr))
        else
            val selected = selectTrigger(deps)
            selected.foreach { dep => reduceDep(dep) }

    private def reduceComponentsForModule(module: LambdaModule): Unit =
        val calls = allCmpsPerFn(module)
        if tooManyContexts(calls) then reduceContextsForModule(module)
        else getParentModule(calls.head.clo) match {
            case m : LambdaModule => reduceComponentsForModule(m)
            case MainModule       => return
        }

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
        val selected = selectAddress(groupByLocation)
        selected.foreach { case (loc, addrs) => reduceAddressesForLocation(loc, addrs) }

    private def reduceAddressesForLocation(loc: Expression, addrs: Set[Addr]): Unit =
        //debug(s"Reducing ${addrs.size} addrs")
        getAddrModule(addrs.head) match {
            case m : LambdaModule => reduceContextsForModule(m)
            case _                => return
        }

    private def reduceClosures(cls: Set[(SchemeLambdaExp, Environment[Addr])]) =
        val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
        val selected = selectClosure(groupByFunction)
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

    override protected def tooManyComponents(moduleCmps: MultiSet[Component]): Boolean = 
        Random.nextBoolean()
        

    override protected def tooManyDependencies(deps: Set[Dependency]): Boolean = 
        Random.nextBoolean()

    override protected def tooManyContexts(calls: Set[Call[ComponentContext]]): Boolean = 
        Random.nextBoolean()

trait TooManyAlways extends AdaptiveContextSensitivity: 
    // always choose to adapt
    this: AdaptiveContextSensitivityPolicy =>

    override protected def tooManyComponents(moduleCmps: MultiSet[Component]): Boolean = 
        true
        
    override protected def tooManyDependencies(deps: Set[Dependency]): Boolean = 
        true

    override protected def tooManyContexts(calls: Set[Call[ComponentContext]]): Boolean = 
        true

trait TooManyCost extends AdaptiveContextSensitivity:
    // choose when to adapt based on a maximum cost
    this: AdaptiveContextSensitivityPolicy => 
    override protected def tooManyComponents(moduleCmps: MultiSet[Component]): Boolean = 
        val numberOfComponents = moduleCmps.distinctCount
        val maximumComponentCost = moduleCmps.content.maxBy(_._2)._2
        numberOfComponents > maximumComponentCost

    override protected def tooManyDependencies(deps: Set[Dependency]): Boolean = 
        val numberOfDependencies = deps.size
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        numberOfDependencies > maximumDependencyCost

    override protected def tooManyContexts(calls: Set[Call[ComponentContext]]): Boolean = 
        val groupedByClo = calls.groupBy(_.clo)
        val cloMaxContexts = groupedByClo.maxBy(_._2.size)._2.size
        cloMaxContexts > groupedByClo.size

trait SelectRandom extends AdaptiveContextSensitivity: 
    // select randomly
    this: AdaptiveContextSensitivityPolicy =>
    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        Random.shuffle(cmpsPerFn.toList).take(1)

    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        Random.shuffle(moduleCmps.toList).take(1)

    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = 
        Random.shuffle(groupedByLoc.toList).take(1)

    override protected def selectTrigger(deps: Set[Dependency]) = 
        Random.shuffle(deps.toList).take(1)

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = 
        Random.shuffle(groupByFunction.toList).take(1)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = 
        Random.shuffle(groupByLocation.toList).take(1)

trait SelectMostContexts extends AdaptiveContextSensitivity: 
    // select the components with the most different contexts
    this: AdaptiveContextSensitivityPolicy =>
    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, _._2.cardinality)

    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        val maximumComponentCost = moduleCmps.content.maxBy(_._2)._2
        selectLargest[(Component, Int)](moduleCmps.content, _._2, maximumComponentCost)
    
    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = 
        selectLargest[(Expression, Set[Dependency])](groupedByLoc, _._2.size)

    override protected def selectTrigger(deps: Set[Dependency]) = 
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        selectLargest[Dependency](deps, depCounts, maximumDependencyCost)

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = 
        selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = 
        selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)

trait SelectMostDependencies extends AdaptiveContextSensitivity: 
    // select the components with the most dependencies
    this: AdaptiveContextSensitivityPolicy => 

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 depsPerCmp.getOrElse(cmp, Set.empty).size + x} ))

    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        selectLargest[(Component, Int)](moduleCmps.content, (cmp: Component, _: Int) => depsPerCmp.getOrElse(cmp, Set.empty).size)
    
    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = 
        selectLargest[(Expression, Set[Dependency])](groupedByLoc, _._2.size)

    override protected def selectTrigger(deps: Set[Dependency]) = 
        selectLargest[Dependency](deps, depCounts)

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = 
        selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = 
        selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)

trait SelectLeastDependencies extends AdaptiveContextSensitivity: 
    // select the components with the least dependencies
    this: AdaptiveContextSensitivityPolicy => 

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 - (depsPerCmp.getOrElse(cmp, Set.empty).size + x)} ))

    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        selectLargest[(Component, Int)](moduleCmps.content, (cmp: Component, _: Int) => - depsPerCmp.getOrElse(cmp, Set.empty).size)
    
    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = 
        selectLargest[(Expression, Set[Dependency])](groupedByLoc, (_: Expression, deps: Set[Dependency]) => - deps.size)

    override protected def selectTrigger(deps: Set[Dependency]) = 
        selectLargest[Dependency](deps, depCounts mapValues {(v: Int) => - v})

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = 
        selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = 
        selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)

trait SelectMostContextsLeastDependencies extends AdaptiveContextSensitivity: 
    // select the components with the most contexts and from those the ones with the least dependencies
    // TODO: this could probably be implemented smarter (now, very little is adapted because we filter twice)
    this: AdaptiveContextSensitivityPolicy => 

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        def mostContexts = selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, _._2.cardinality)
        selectLargest[(SchemeModule, MultiSet[Component])](mostContexts, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 - (depsPerCmp.getOrElse(cmp, Set.empty).size + x)} ))

    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        val maximumComponentCost = moduleCmps.content.maxBy(_._2)._2
        def mostContexts = selectLargest[(Component, Int)](moduleCmps.content, _._2, maximumComponentCost)
        selectLargest[(Component, Int)](mostContexts, (cmp: Component, _: Int) => - depsPerCmp.getOrElse(cmp, Set.empty).size)
    
    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = 
        def mostContexts = selectLargest[(Expression, Set[Dependency])](groupedByLoc, _._2.size)
        selectLargest[(Expression, Set[Dependency])](mostContexts, (_: Expression, deps: Set[Dependency]) => - deps.size)

    override protected def selectTrigger(deps: Set[Dependency]) = 
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        def mostContexts = selectLargest[Dependency](deps, depCounts, maximumDependencyCost)
        selectLargest[Dependency](mostContexts, depCounts mapValues {(v: Int) => - v})

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = 
        selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = 
        selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)

trait SelectImprecise extends AdaptiveContextSensitivity: 
    // select the components with the most imprecise values (ie highest in the lattice)
    this: AdaptiveContextSensitivityPolicy => 

    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                                            cmps.distinctElements.foldLeft(0) { (x: Int, cmp: Component) => 
                                                                                                                 x + lattice.elementSize(returnValue(cmp))} ))

    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        selectLargest[(Component, Int)](moduleCmps.toMap, (cmp: Component, _: Int) => lattice.elementSize(returnValue(cmp)))
    
    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = // TODO
        selectLargest[(Expression, Set[Dependency])](groupedByLoc, (_: Expression, deps: Set[Dependency]) => deps.size)

    override protected def selectTrigger(deps: Set[Dependency]) = // TODO
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        selectLargest[Dependency](deps, depCounts, maximumDependencyCost)

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = // TODO
        selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = // TODO
        selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)

trait SelectDifferentValues extends AdaptiveContextSensitivity: 
    this: AdaptiveContextSensitivityPolicy => 

    // select modules where the contexts do not represent many different abstract values
    override protected def selectStartingModule(cmpsPerFn: Map[SchemeModule, MultiSet[Component]]) = 
        selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, ((_: SchemeModule, cmps: MultiSet[Component]) => 
                                                            Math.round((MultiSet(cmps.content.map({ (cmp: Component, x: Int) => (returnValue(cmp), x)}), 
                                                                     cmps.cardinality).distinctCount / cmps.cardinality) * 10)
                                                              ))
    // select components that have imprecise values as well as many contexts
    override protected def selectComponent(moduleCmps:  MultiSet[Component]) = 
        selectLargest[(Component, Int)](moduleCmps.toMap, (cmp: Component, x: Int) => Math.round((lattice.elementSize(returnValue(cmp)) + (x / moduleCmps.cardinality)) * 10))
    
    override protected def selectReanalysis(groupedByLoc: Map[Expression, Set[Dependency]]) = // TODO
        selectLargest[(Expression, Set[Dependency])](groupedByLoc, (_: Expression, deps: Set[Dependency]) => deps.size)

    override protected def selectTrigger(deps: Set[Dependency]) = // TODO
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        selectLargest[Dependency](deps, depCounts, maximumDependencyCost)

    override protected def selectClosure(groupByFunction: Map[SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])]]) = // TODO
        selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)

    override protected def selectAddress(groupByLocation: Map[Expression, Set[Addr]]) = // TODO
        selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)

