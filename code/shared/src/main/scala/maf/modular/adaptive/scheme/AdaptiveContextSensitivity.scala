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

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics:
    this: AdaptiveContextSensitivityPolicy =>

    // disable warning messages and debug logging by default (can be override for custom logging)
    protected def warn(message: => String): Unit = ()
    protected def debug(message: => String): Unit = ()

    // extra parameters to control the "aggressiveness" of the adaptation (TODO: in Scala 3, make these (default?) trait parameters):
    // - `reduceFactor`: determines by what factor the number of components needs to be reduced
    // - `cutoffFactor`: determines the cutoff for selecting "culprits" to be reduced in the adaptation
    val reduceFactor = 0.5
    val cutoffFactor = 0.5

    // use a different context-sensitivity policy per closure

    private var policyPerFn: Map[LambdaModule, ContextSensitivityPolicy] = Map.empty
    private def getCurrentPolicy(fn: LambdaModule): ContextSensitivityPolicy =
      policyPerFn.getOrElse(fn, defaultPolicy)
    private def setCurrentPolicy(fn: LambdaModule, ply: ContextSensitivityPolicy): Unit =
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

    private var allCmpsPerFn: Map[LambdaModule, Set[Call[ComponentContext]]] = Map.empty
    private var cmpsPerFn: Map[SchemeModule, MultiSet[Component]] = Map.empty
    private var depsPerCmp: Map[Component, Set[Dependency]] = Map.empty
    private var depCounts: Map[Dependency, Int] = Map.empty

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

    private var reducedModules: Set[SchemeModule] = Set.empty
    private var reducedDeps: Set[Dependency] = Set.empty

    // adapting the analysis

    def modulesToAdapt =
      selectLargest[(SchemeModule, MultiSet[Component])](cmpsPerFn, _._2.cardinality)

    def inspect() =
        // start the adaptation
        modulesToAdapt.foreach { case (module, _) =>
          reduceModule(module)
        }
        // update the analysis
        if reducedModules.nonEmpty then { adaptAnalysis() }
        // clear the set of reduced modules
        reducedModules = Set.empty
        reducedDeps = Set.empty

    private def reduceModule(module: SchemeModule) =
        val moduleCmps = cmpsPerFn(module)
        val numberOfComponents = moduleCmps.distinctCount
        val maximumComponentCost = moduleCmps.content.maxBy(_._2)._2
        if numberOfComponents > maximumComponentCost then reduceComponentsForModule(module.asInstanceOf[LambdaModule])
        else
            val selectedCmps = selectLargest[(Component, Int)](
              moduleCmps.content,
              _._2,
              maximumComponentCost
            )
            selectedCmps.foreach { case (cmp, _) => reduceReanalysesForComponent(cmp) }

    private def reduceReanalysesForComponent(cmp: Component) =
        val deps = depsPerCmp(cmp)
        val groupedByLoc = deps.groupBy(getDepExp)
        val selected = selectLargest[(Expression, Set[Dependency])](groupedByLoc, _._2.size)
        selected.foreach { case (loc, deps) => reduceTriggersForLocation(loc, deps) }

    private def reduceTriggersForLocation(loc: Expression, deps: Set[Dependency]) =
        val numberOfDependencies = deps.size
        val maximumDependencyCost = depCounts(deps.maxBy(depCounts))
        if numberOfDependencies > maximumDependencyCost then reduceAddressesForLocation(loc, deps.map(_.asInstanceOf[AddrDependency].addr))
        else
            val selected = selectLargest[Dependency](deps, depCounts, maximumDependencyCost)
            selected.foreach { dep => reduceDep(dep) }

    private def reduceComponentsForModule(module: LambdaModule): Unit =
        val calls = allCmpsPerFn(module)
        val groupedByClo = calls.groupBy(_.clo)
        val cloMaxContexts = groupedByClo.maxBy(_._2.size)._2.size
        if cloMaxContexts > groupedByClo.size then reduceContextsForModule(module)
        else reduceComponentsForModule(getParentModule(calls.head.clo).asInstanceOf[LambdaModule])

    // find a fitting policy
    private def reduceContextsForModule(module: LambdaModule): Unit =
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

    private def reduceAddressesForLocation(loc: Expression, addrs: Set[Addr]) =
      //debug(s"Reducing ${addrs.size} addrs")
      reduceContextsForModule(getAddrModule(addrs.head).asInstanceOf[LambdaModule])

    private def reduceClosures(cls: Set[(SchemeLambdaExp, Environment[Addr])]) =
        val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
        val selected = selectLargest[(SchemeLambdaExp, Set[(SchemeLambdaExp, Environment[Addr])])](groupByFunction, _._2.size)
        selected.foreach { case (fn, closures) => reduceClosuresForFunction(fn, closures) }

    private def reduceClosuresForFunction(fn: SchemeLambdaExp, closures: Set[(SchemeLambdaExp, Environment[Addr])]) =
      //debug(s"Reducing ${closures.size} closures")
      reduceContextsForModule(getParentModule(closures.head).asInstanceOf[LambdaModule])

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
      selectLargest(data, size, size(data.maxBy(size)))
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
    private def sizeOfV(v: ValueElement): Int = v match
        case modularLatticeWrapper.modularLattice.Pointer(ptrs)    => ptrs.size
        case modularLatticeWrapper.modularLattice.Clo(closures)    => closures.size
        case modularLatticeWrapper.modularLattice.Cons(car, cdr)   => sizeOfValue(car) + sizeOfValue(cdr)
        case modularLatticeWrapper.modularLattice.Vec(_, elements) => elements.map(_._2).map(sizeOfValue).sum
        case _                                                     => 0
