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
    type Dlt = LocalStore[Adr, Val]#Delta
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

    // in reality, 'Any' here is `Set[(Val, cmp.sto.Delta)]` for a given key `cmp `
    var results: Map[Component, Any] = Map.empty

    case class ResultDependency(cmp: Component) extends Dependency

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): sto.Delta = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): sto.Delta = sto.update(adr, vlu)

    def eqA(sto: Sto, anl: Anl): MaybeEq[Adr] = new MaybeEq[Adr]:
        def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
            if a1 == a2 then
                if sto.lookupCount(a1) == CountOne then BoolLattice[B].inject(true)
                else BoolLattice[B].top
            else BoolLattice[B].inject(false)

    def withRestrictedStore(rs: Set[Adr])(blk: A[Val]): A[Val] =
        (anl, env, sto, ctx, tai) =>
            val gcs = sto.collect(rs)
            blk(anl, env, gcs, ctx, true).map { (v, d, u) =>
                val gcd = d.collect(lattice.refs(v) ++ u)
                (v, sto.replay(gcd, tai), u)
            }

    import analysisM._
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

    //
    // ANALYSISM MONAD
    //

    type A[X] = (anl: Anl, env: Env, sto: Sto, ctx: Ctx, tai: Boolean) => Set[(X, sto.Delta, Set[Adr])]

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
            (_, _, sto, _, _) => Set((x, sto.emptyDelta, Set.empty))
        def map[X, Y](m: A[X])(f: X => Y) =
            (anl, env, sto, ctx, tai) => m(anl, env, sto, ctx, tai).map((x, d, u) => (f(x), d, u))
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (anl, env, sto, ctx, tai) =>
                for
                    (x0, d0, u0) <- m(anl, env, sto, ctx, tai)
                    (x1, d1, u1) <- f(x0)(anl, env, sto.integrate(d0), ctx, tai)
                yield (x1, sto.compose(d1, d0), u0 ++ u1.filter(sto.contains))
        // MONADJOIN
        def mbottom[X] =
            (_, _, _, _, _) => Set.empty
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
            (anl, env, sto, ctx, tai) => x(anl, env, sto, ctx, tai) ++ y(anl, env, sto, ctx, tai)
        // MONADERROR
        def fail[X](err: Error) =
            mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
            (anl, _, sto, _, _) => Set((eqA(sto, anl), sto.emptyDelta, Set.empty))
        def extendSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _, _) => Set(((), extendV(sto, adr, vlu), Set.empty))
        def updateSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _, _) => Set(((), updateV(sto, adr, vlu), Set(adr)))
        def lookupSto(adr: Adr) =
            flatMap((anl, _, sto, _, _) => Set((sto.lookupValue(adr), sto.emptyDelta, Set.empty)))(inject)
        // CTX STUFF
        def getCtx =
            (_, _, sto, ctx, _) => Set((ctx, sto.emptyDelta, Set.empty))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, sto, ctx, tai) => blk(anl, env, sto, f(ctx), tai)
        // ENV STUFF
        def getEnv =
            (_, env, sto, _, _) => Set((env, sto.emptyDelta, Set.empty))
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, sto, ctx, tai) => blk(anl, f(env), sto, ctx, tai)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, sto, ctx, _) => anl.call(lam, env, sto, ctx)
        // NONTAIL!
        override def nontail[X](blk: => A[X]): A[X] =
            (anl, env, sto, ctx, _) => blk(anl, env, sto, ctx, false)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new SchemeLocalIntraAnalysis(cmp)
    class SchemeLocalIntraAnalysis(cmp: Cmp) extends IntraAnalysis(cmp):
        intra =>

        // local state
        var results = inter.results

        def call(lam: Lam, env: Env, sto: Sto, ctx: Ctx): Set[(Val, sto.Delta, Set[Adr])] =
            val cmp = CallComponent(lam, env, sto, ctx)
            spawn(cmp)
            register(ResultDependency(cmp))
            results.getOrElse(cmp, Set.empty).asInstanceOf[Set[(Val, sto.Delta, Set[Adr])]]

        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            val res = eval(cmp.exp)(this, cmp.env, cmp.sto, cmp.ctx, true)
            val rgc = res.map((v, d, u) => (v, d.collect(lattice.refs(v) ++ u), u))
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
