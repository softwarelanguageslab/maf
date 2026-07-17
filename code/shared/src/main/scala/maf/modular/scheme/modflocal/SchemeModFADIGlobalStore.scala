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

abstract class SchemeModFADIGlobalStore(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
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

    //
    // COMPONENTS
    //

    sealed trait Component extends Serializable:
        val exp: Exp
        val env: Env
        val ctx: Ctx
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, ctx: Ctx) extends Component:
        val exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName}@${lam.idn} [$ctx]"

    def initialComponent: Cmp = MainComponent
    def expr(cmp: Cmp): Exp = cmp.exp

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

    //
    // ANALYSISM MONAD
    //

    type A[X] = (anl: Anl, env: Env, ctx: Ctx) => Option[X]

    protected def analysisM: AnalysisM[A] = new AnalysisM[A]:
        // MONAD
        def unit[X](x: X) =
            (_, _, _) => Some(x)
        def map[X, Y](m: A[X])(f: X => Y) =
            (anl, env, ctx) => m(anl, env, ctx).map(f)
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (anl, env, ctx) =>
                for
                    x0 <- m(anl, env, ctx)
                    x1 <- f(x0)(anl, env, ctx)
                yield x1
        // MONADJOIN
        def mbottom[X] =
            (_, _, _) => None
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
            (anl, env, ctx) => 
                (x(anl, env, ctx), y(anl, env, ctx)) match
                    case (res1, None) => res1
                    case (None, res2) => res2
                    case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1, res2))
        // MONADERROR
        def fail[X](err: Error) =
            mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def extendSto(adr: Adr, vlu: Val) = 
            (anl, _, _) => anl.writeAddr(adr, vlu) 
        def updateSto(adr: Adr, vlu: Val) = 
            (anl, _, _) => anl.writeAddr(adr, vlu)
        def lookupSto(adr: Adr) =
            (anl, _, _) => anl.lookupAddr(adr)
        def addrEq: A[MaybeEq[Adr]] = // NOTE: I don't think addrEq is actually used?
            (anl, _, _) => Some(anl.globalStore.addrEq)
        // CTX STUFF
        def getCtx =
            (_, _, ctx) => Some(ctx)
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, ctx) => blk(anl, env, f(ctx))
        // ENV STUFF
        def getEnv =
            (_, env, _) => Some(env)
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, ctx) => blk(anl, f(env), ctx)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, ctx) => anl.call(lam, env, ctx)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new SchemeLocalIntraAnalysis(cmp)
    class SchemeLocalIntraAnalysis(cmp: Cmp) extends IntraAnalysis(cmp):
        intra =>

        // local state
        var results = inter.results
        var globalStore = inter.globalStore

        def call(lam: Lam, env: Env, ctx: Ctx): Option[Val] =
            val cmp = CallComponent(lam, env, ctx)
            spawn(cmp)
            register(ResultDependency(cmp))
            results.get(cmp)

        def writeAddr(adr: Adr, vlu: Val): Option[Unit] =
            globalStore.extendOption(adr, vlu) match
                case None => 
                    Some(()) // nothing to do ...
                case Some(upd) =>
                    globalStore = upd 
                    trigger(AddrDependency(adr))
                    Some(())

        def lookupAddr(adr: Adr): Option[Val] =
            register(AddrDependency(adr))
            globalStore.get(adr)
            
        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            val rgc = eval(cmp.exp)(this, cmp.env, cmp.ctx)
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
            case _ => super.doWrite(dep)


//
// track results (for precision measurements, soundness tests, ...)
//

trait SchemeModFADIGlobalStoreAnalysisResults extends SchemeModFADIGlobalStore with AnalysisResults[SchemeExp]:
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

class SchemeModFADIGlobalStoreAnalysis(prg: SchemeExp, k: Int)
    extends SchemeModFADIGlobalStore(prg)
    with SchemeConstantPropagationDomain
    with SchemeModFLocalCallSiteSensitivity(k)
    with maf.modular.worklist.FIFOWorklistAlgorithm[SchemeExp]
        //override def run(t: maf.util.benchmarks.Timeout.T) = 
        //    super.run(t)
        //    println(results(MainComponent))
