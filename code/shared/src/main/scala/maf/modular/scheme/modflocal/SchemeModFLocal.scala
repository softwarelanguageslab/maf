package maf.modular.scheme.modflocal

import maf.modular._
import maf.modular.scheme._
import maf.core._
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

abstract class SchemeModFLocal(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
    inter: SchemeDomain with SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount
    type Anl = SchemeLocalIntraAnalysis

    //
    // INITIALISATION
    //

    lazy val initialExp: Exp = program
    lazy val initialEnv: Env = Environment(initialBds.map(p => (p._1, p._2)))
    lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))

    given shouldCount: (Adr => Boolean) =
        case _: PtrAddr[_] => true
        case _             => false

    private lazy val initialBds: Iterable[(String, Adr, Val)] =
        primitives.allPrimitives.view
            .filterKeys(initialExp.fv)
            .map { case (name, p) => (name, PrmAddr(name), lattice.primitive(p.name)) }

    //
    // COMPONENTS
    //

    sealed trait Component extends Serializable:
        val exp: Exp
        val env: Env
        val sto: Sto
        val ctx: Ctx
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        val sto = initialSto
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, sto: Sto, ctx: Ctx) extends Component:
        val exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName}@${lam.idn} [$ctx] [${sto.content.hc}]"

    def initialComponent: Cmp = MainComponent
    def expr(cmp: Cmp): Exp = cmp.exp

    //
    // RESULTS
    //

    var results: Map[Component, Set[(Val, Dlt, Set[Adr], Set[Adr])]] = Map.empty

    case class ResultDependency(cmp: Component) extends Dependency

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.update(adr, vlu)

    def eqA(sto: Sto): MaybeEq[Adr] = new MaybeEq[Adr]:
        def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
            if a1 == a2 then
                if sto.lookupCount(a1) == CountOne 
                then BoolLattice[B].inject(true)
                else BoolLattice[B].top
            else BoolLattice[B].inject(false)

    def withRestrictedStore(rs: Set[Adr])(blk: A[Val]): A[Val] =
        (anl, env, sto, ctx) =>
            val gcs = LocalStoreGC().collect(sto, rs)
            blk(anl, env, gcs, ctx).map { (v, d, u, a) =>
                val gcu = u.filter(gcs.contains)
                val gca = a.filter(d.delta.contains)
                val gcd = DeltaGC(gcs).collect(d, lattice.refs(v) ++ gcu)
                (v, gcd, gcu, gca)
            }

    import analysisM_._
    override def eval(exp: Exp): A[Val] =
        withEnv(_.restrictTo(exp.fv)) {
            getEnv >>= { env =>
                withRestrictedStore(env.addrs) {
                    super.eval(exp)
                }
            }
        }

    override protected def applyPrimitive(app: App, prm: Prim, ags: List[Val]): A[Val] =
        withRestrictedStore(ags.flatMap(lattice.refs).toSet) {
            super.applyPrimitive(app, prm, ags)
        }

    override protected def applyClosure(app: App, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
        withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
            super.applyClosure(app, lam, ags, fvs)
        }

    override protected def nontail[X](blk: => A[X]) =  
        (anl, env, sto, ctx) =>
            blk(anl, env, sto, ctx).map { (v, d, u, a) =>
                (v, sto.replay(d, a), u, a)
            }
    //
    // ANALYSISM MONAD
    //

    type A[X] = (anl: Anl, env: Env, sto: Sto, ctx: Ctx) => Set[(X, Dlt, Set[Adr], Set[Adr])]

    protected def analysisM: AnalysisM[A] = new AnalysisM[A]:
        // MONAD
        def unit[X](x: X) =
            (_, _, sto, _) => Set((x, Delta.emptyDelta[Adr,Val], Set.empty, Set.empty))
        def map[X, Y](m: A[X])(f: X => Y) =
            (anl, env, sto, ctx) => m(anl, env, sto, ctx).map((x, d, u, a) => (f(x), d, u, a))
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (anl, env, sto, ctx) =>
                for
                    (x0, d0, u0, a0) <- m(anl, env, sto, ctx)
                    (x1, d1, u1, a1) <- f(x0)(anl, env, sto.integrate(d0), ctx)
                yield (x1, Delta.compose(d1, d0), u0 ++ u1, a0 ++ a1)
        // MONADJOIN
        def mbottom[X] =
            (_, _, _, _) => Set.empty
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
            (anl, env, sto, ctx) => x(anl, env, sto, ctx) ++ y(anl, env, sto, ctx)
        // MONADERROR
        def fail[X](err: Error) =
            mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
            (anl, _, sto, _) => Set((eqA(sto), Delta.emptyDelta, Set.empty, Set.empty))
        def extendSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Set(((), extendV(sto, adr, vlu), Set.empty, Set(adr)))
        def updateSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Set(((), updateV(sto, adr, vlu), Set(adr), Set.empty))
        def lookupSto(adr: Adr) =
            flatMap((anl, _, sto, _) => Set((sto.lookupValue(adr), Delta.emptyDelta, Set.empty, Set.empty)))(inject)
        // CTX STUFF
        def getCtx =
            (_, _, sto, ctx) => Set((ctx, Delta.emptyDelta, Set.empty, Set.empty))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, sto, ctx) => blk(anl, env, sto, f(ctx))
        // ENV STUFF
        def getEnv =
            (_, env, sto, _) => Set((env, Delta.emptyDelta, Set.empty, Set.empty))
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, sto, ctx) => blk(anl, f(env), sto, ctx)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, sto, ctx) => anl.call(lam, env, sto, ctx)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new SchemeLocalIntraAnalysis(cmp)
    class SchemeLocalIntraAnalysis(cmp: Cmp) extends IntraAnalysis(cmp):
        intra =>

        // local state
        var results = inter.results

        def call(lam: Lam, env: Env, sto: Sto, ctx: Ctx): Set[(Val, Dlt, Set[Adr], Set[Adr])] =
            val cmp = CallComponent(lam, env, sto, ctx)
            spawn(cmp)
            register(ResultDependency(cmp))
            results.getOrElse(cmp, Set.empty)

        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            val rgc = eval(cmp.exp)(this, cmp.env, cmp.sto, cmp.ctx)
            val old = results.getOrElse(cmp, Set.empty)
            if rgc != old then
                intra.results += cmp -> rgc
                trigger(ResultDependency(cmp))

        override def doWrite(dep: Dependency): Boolean = dep match
            case ResultDependency(cmp) =>
                val old = inter.results.getOrElse(cmp, Set.empty)
                val cur = intra.results(cmp)
                if old != cur then
                    inter.results += cmp -> cur
                    true
                else false
            case _ => super.doWrite(dep)

trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

    override def extendV(sto: Sto, adr: Adr, vlu: Val) =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
                resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
            case _ => ()
        super.extendV(sto, adr, vlu)

    override def updateV(sto: Sto, adr: Adr, vlu: Val) =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
                resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
            case _ => ()
        super.updateV(sto, adr, vlu)

// a standard instance

class SchemeDSSAnalysis(prg: SchemeExp, k: Int)
    extends SchemeModFLocal(prg)
    with SchemeConstantPropagationDomain
    with SchemeModFLocalCallSiteSensitivity(k)
    with maf.modular.worklist.FIFOWorklistAlgorithm[SchemeExp]
