package maf.modular.scheme.modflocal

import maf.modular._
import maf.modular.scheme._
import maf.core.*
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.util.benchmarks.Timeout
import maf.language.CScheme._
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.LatticeWithAddrs
import akka.actor.ProviderSelection.Local
import maf.util.datastructures.SmartMap
import maf.modular.scheme.modf.SchemeModFComponent.Call
import maf.core.Monad.MonadSyntaxOps
import maf.util.Wrapper
import maf.util.Wrapper.*
import maf.core.Store.{CountingStore, given}

abstract class SchemeModFConcreteDeps(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
    inter: SchemeDomain & SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = Store.SimpleStore[Adr, Val]
    type Anl = SchemeLocalIntraAnalysis

    given store: Store[Sto, Adr, Val] = Store.simpleInstance

    //
    // INITIALISATION
    //

    lazy val initialExp: Exp = program
    lazy val initialEnv: Env = Environment(initialBds.map(p => (p._1, p._2)))
    lazy val initialSto: Sto = initialBds.foldLeft(store.empty) {
        case (accSto, (_, adr, vlu)) => accSto.extend(adr, vlu)
    }

    private lazy val initialBds: Iterable[(String, Adr, Val)] =
        primitives.allPrimitives.view
            .filterKeys(initialExp.fv)
            .map { case (name, p) => (name, PrmAddr(name), lattice.primitive(p.name)) }

    override def init(): Unit = 
        super.init()
        globalStore = initialSto

        ctrlDeps = initialCtrlDeps
        dataDeps = initialDataDeps

    //
    // COMPONENTS
    //

    sealed trait Component extends Serializable:
        val exp: Exp
        val env: Env
        val ctx: Ctx
        val ctrl: Ctrl
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        val ctrl = initialCtrl
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, ctx: Ctx, ctrl: Ctrl) extends Component:
        val exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName}@${lam.idn} [$ctx]"

    def initialComponent: Cmp = MainComponent
    def expr(cmp: Cmp): Exp = cmp.exp
   
    //
    // SLICING DEPENDENCIES
    //

    type Ctrl = Option[SchemeExp]
    type CtrlDeps = Map[SchemeExp, Ctrl] // the slicing control dependency
    case class CtrlDependency(exp: SchemeExp) extends Dependency // for the MAF dependency tracking
    
    lazy val initialCtrlDeps: CtrlDeps = Map.empty
    lazy val initialCtrl: Ctrl = None
    var ctrlDeps: CtrlDeps = _ 

    type DataDep = Address
    type DataDeps = Map[SchemeExp, Set[DataDep]]
    case class DataDependency(exp: SchemeExp) extends Dependency 

    lazy val initialDataDeps: DataDeps = Map.empty 
    var dataDeps: DataDeps = _ 

    //
    // RESULTS
    //

    var results: Map[Component, Val] = Map.empty

    case class ResultDependency(cmp: Component) extends Dependency

    //
    // STORE STUFF
    //

    var globalStore: Sto = _

    def writeAddr(adr: Adr, vlu: Val) = 
        globalStore = globalStore.extend(adr, vlu)

    case class AddrDependency(adr: Adr) extends Dependency

    import analysisM_._

    override def eval(exp: Exp): A[Val] = 
        for 
            ctrl <- getCtrl
            _ <- addCtrlDep(exp, ctrl)       
            (resVal, deps) <- deps(super.eval(exp)) 
            _ <- saveDeps(exp, deps)
            res <- unitWithDeps(deps)(resVal)
        yield res

    override def evalAll(lst: List[SchemeExp]): A[List[Val]] = 
        lst match
            case Nil         => unit(Nil)
            case exp :: Nil  => eval(exp).map(_ :: Nil)
            case exp :: exps =>
                for
                    (v, vDeps)  <- nontailKeepEnv    { deps(eval(exp)) }
                    (vs, vsDeps) <- nontailKeepVal(v) { deps(evalAll(exps)) }
                    res <- unitWithDeps(vDeps ++ vsDeps)(v::vs)
                yield res

    override protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): A[Val] = 
        for
            (cnd, cndDeps) <- nontailKeepEnv { deps(eval(prd)) }
            (resVal, resDeps) <- deps(withCtrl(_ => Some(prd)){ cond(cnd, eval(csq), eval(alt)) })
            res <- unitWithDeps(cndDeps ++ resDeps)(resVal)
        yield res

    override protected def evalCall(app: App): A[Val] =
        for
            (fun, funDeps) <- nontailKeepEnv { deps(eval(app.f)) }
            (ags, agsDeps) <- nontailKeepVal(fun) { deps(evalAll(app.args)) }
            (resVal, resDeps) <- deps(applyFun(app, fun, ags))
            res <- unitWithDeps(funDeps ++ agsDeps ++ resDeps)(resVal)
        yield res

 
    //
    // ANALYSISM MONAD
    //

    type A[X] = (anl: Anl, env: Env, ctx: Ctx, ctrl: Ctrl) => Option[(X, Set[DataDep])]

    extension [X](m: A[X])
        def withFilter(p: X => Boolean): A[X] = 
            (anl, env, ctx, ctrl) => 
                m(anl, env, ctx, ctrl) match
                    case None => None 
                    case Some(res, deps) => 
                        if p(res) then Some(res, deps)
                        else None

    // CONTROL DEPENDENCY STUFF
    private def getCtrl: A[Ctrl] = 
            (_, _, _, ctrl) => Some((ctrl, Set.empty)) 

    private def withCtrl[X](f: Ctrl => Ctrl)(blk: A[X]): A[X] = 
        (anl, env, ctx, ctrl) => blk(anl, env, ctx, f(ctrl))

    private def addCtrlDep(exp: SchemeExp, ctrl: Ctrl): A[Unit] =
            (anl, env, ctx, ctrl) => anl.addCtrlDep(exp, ctrl)

    // DATA DEPENDENCY STUFF
    private def deps[X](blk: A[X]): A[(X, Set[DataDep])] = 
        (anl, env, ctx, ctrl) => blk(anl, env, ctx, ctrl).map((x, deps) => ((x, deps), deps))

    private def saveDeps[X](exp: SchemeExp, deps: Set[DataDep]): A[Unit] = 
        (anl, env, ctx, ctrl) => anl.addDataDeps(exp, deps)

    private def unitWithDeps[X](deps: Set[DataDep])(x: X): A[X] =  
        (_, _, _, _) => Some((x, deps))


    protected def analysisM: AnalysisM[A] = new AnalysisM[A]:
        // MONAD
        def unit[X](x: X) =
            (_, _, _, _) => Some((x, Set.empty))
        def map[X, Y](m: A[X])(f: X => Y) =
            (anl, env, ctx, ctrl) => m(anl, env, ctx, ctrl).map((res, deps) => (f(res), deps))
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (anl, env, ctx, ctrl) =>
                for
                    (x0, deps0) <- m(anl, env, ctx, ctrl)
                    (x1, deps1) <- f(x0)(anl, env, ctx, ctrl)
                yield (x1, deps1)
                
        // MONADJOIN
        def mbottom[X] =
            (_, _, _, _) => None
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
            (anl, env, ctx, ctrl) => 
                (x(anl, env, ctx, ctrl), y(anl, env, ctx, ctrl)) match
                    case (res1, None) => res1
                    case (None, res2) => res2
                    case (Some((res1, deps1)), Some((res2, deps2))) => Some((Lattice[X].join(res1, res2), deps1 ++ deps2))
        // MONADERROR
        def fail[X](err: Error) =
            mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def extendSto(adr: Adr, vlu: Val) = 
            (anl, _, _, _) => anl.writeAddr(adr, vlu) 
        def updateSto(adr: Adr, vlu: Val) = 
            (anl, _, _, _) => anl.writeAddr(adr, vlu)
        def lookupSto(adr: Adr) =
            (anl, _, _, _) => anl.lookupAddr(adr)
        def addrEq: A[MaybeEq[Adr]] = // NOTE: I don't think addrEq is actually used?
            (anl, _, _, _) => Some((anl.globalStore.addrEq, Set.empty))
        // CTX STUFF
        def getCtx =
            (_, _, ctx, _) => Some((ctx, Set.empty))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, ctx, ctrl) => blk(anl, env, f(ctx), ctrl)
        // ENV STUFF
        def getEnv =
            (_, env, _, _) => Some((env, Set.empty))
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, ctx, ctrl) => blk(anl, f(env), ctx, ctrl)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, ctx, ctrl) => anl.call(lam, env, ctx, ctrl)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new SchemeLocalIntraAnalysis(cmp)
    class SchemeLocalIntraAnalysis(cmp: Cmp) extends IntraAnalysis(cmp):
        intra =>

        // local state
        var results = inter.results
        var globalStore = inter.globalStore
        var ctrlDeps = inter.ctrlDeps
        var dataDeps = inter.dataDeps


        def call(lam: Lam, env: Env, ctx: Ctx, ctrl: Ctrl): Option[(Val, Set[DataDep])] =
            val cmp = CallComponent(lam, env, ctx, ctrl)
            spawn(cmp)
            register(ResultDependency(cmp))
            for
                res <- results.get(cmp)
                deps = dataDeps(lam)
            yield (res, deps)

        def addCtrlDep(exp: SchemeExp, ctrl: Ctrl): Option[(Unit, Set[DataDep])] =
            ctrlDeps += exp -> ctrl 
            trigger(CtrlDependency(exp))
            Some((), Set.empty)

        def addDataDeps(exp: SchemeExp, deps: Set[DataDep]): Option[(Unit, Set[DataDep])] =
            dataDeps += (exp -> (dataDeps.getOrElse(exp, Set.empty) ++ deps))
            // println("added deps: " + exp + " -> " + deps)
            trigger(DataDependency(exp))
            Some((), Set.empty)

        def writeAddr(adr: Adr, vlu: Val): Option[(Unit, Set[DataDep])] =
            globalStore.extendOption(adr, vlu) match
                case None => 
                    Some((), Set.empty) // nothing to do ...
                case Some(upd) =>
                    globalStore = upd 
                    trigger(AddrDependency(adr))
                    Some((), Set.empty)

        def lookupAddr(adr: Adr): Option[(Val, Set[DataDep])] =
            register(AddrDependency(adr))
            for
                v <- globalStore.get(adr)
                deps = Set(adr)
            yield (v, deps)
            
        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            val rgc = eval(cmp.exp)(this, cmp.env, cmp.ctx, cmp.ctrl).map(_._1)
            val old = results.get(cmp)
            if rgc != old then
                intra.results += cmp -> rgc.get
                trigger(ResultDependency(cmp))

        override def doWrite(dep: Dependency): Boolean = dep match
            case ResultDependency(cmp) =>
                val old = inter.results.get(cmp)
                val cur = intra.results(cmp)
                if old != cur then
                    inter.results += cmp -> cur
                    true
                else false
            case AddrDependency(adr) =>
                val old = inter.globalStore(adr)
                val cur = intra.globalStore(adr)
                if old != cur then
                    inter.writeAddr(adr, cur)
                    true
                else false 
            case CtrlDependency(exp) =>
                val old = inter.ctrlDeps.getOrElse(exp, None)
                val cur = intra.ctrlDeps(exp)
                if old != cur then
                    inter.ctrlDeps += exp -> cur
                    true
                else false 
            case DataDependency(exp) =>
                val old = inter.dataDeps.getOrElse(exp, None)
                val cur = intra.dataDeps(exp)
                if old != cur then
                    inter.dataDeps += exp -> cur
                    true
                else false 
            case _ => super.doWrite(dep)


//
// track results (for precision measurements, soundness tests, ...)
//

trait SchemeModFConcreteDepsAnalysisResults extends SchemeModFConcreteDeps with AnalysisResults[SchemeExp]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

    override def writeAddr(adr: Adr, vlu: Val) =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
                resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
            case _ => ()
        super.writeAddr(adr, vlu)


//
// a standard instance
//

class SchemeModFConcreteDepsAnalysis(prg: SchemeExp, k: Int)
    extends SchemeModFConcreteDeps(prg)
    with SchemeConstantPropagationDomain
    with SchemeModFLocalCallSiteSensitivity(k)
    with maf.modular.worklist.RandomWorklistAlgorithm[SchemeExp]:
        // override def run(t: maf.util.benchmarks.Timeout.T) = 
            //println(ctrlDeps)
            //println("data: " + dataDeps)