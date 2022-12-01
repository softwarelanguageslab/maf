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
        def exp: Exp
        def env: Env
        def ctx: Ctx
        def sto: Sto
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        val sto = initialSto
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, sto: Sto, ctx: Ctx) extends Component:
        def exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName}@${lam.idn} [$ctx] [${sto.content.hc}]"

    def initialComponent: Cmp = MainComponent
    def expr(cmp: Cmp): Exp = cmp.exp

    //
    // RESULTS
    //

    var results: Map[Component, Set[(Val, Dlt)]] = Map.empty 

    case class ResultDependency(cmp: Component) extends Dependency

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.update(adr, vlu)

    def eqA(sto: Sto, anl: Anl): MaybeEq[Adr] = new MaybeEq[Adr]:
        def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
            if a1 == a2 then 
                if sto.lookupCount(a1) == CountOne
                then BoolLattice[B].inject(true)
                else BoolLattice[B].top
            else BoolLattice[B].inject(false)

    def withRestrictedStore(rs: Set[Adr])(blk: A[Val]): A[Val] =
        (anl, env, sto, ctx) =>
            val gcs = sto.collect(rs)
            blk(anl, env, gcs, ctx).map { (v, d) =>
                val gcd = d.collect(lattice.refs(v) ++ d.updates)
                (v, sto.replay(gcs, gcd))
            }

    def withRestrictedResult(blk: A[Val]): A[Val] =
        (anl, env, sto, ctx) =>
            blk(anl, env, sto, ctx).map { (v, d) =>
                (v, d.collect(lattice.refs(v) ++ d.updates))
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

    type A[X] = (Anl, Env, Sto, Ctx) => Set[(X, Dlt)]

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
            (_, _, sto, _) => Set((x, Delta.empty(sto)))
        def map[X, Y](m: A[X])(f: X => Y) =
            (anl, env, sto, ctx) => m(anl, env, sto, ctx).map((x, d) => (f(x), d))
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (anl, env, sto, ctx) =>
                for
                    (x0, d0) <- m(anl, env, sto, ctx)
                    (x1, d1) <- f(x0)(anl, env, sto.integrate(d0), ctx)
                yield (x1, sto.compose(d1, d0))
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
            (anl, _, sto, _) => Set((eqA(sto, anl), Delta.empty(sto)))
        def extendSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Set(((), extendV(sto, adr, vlu)))
        def updateSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Set(((), updateV(sto, adr, vlu)))
        def lookupSto(adr: Adr) =
            flatMap((anl, _, sto, _) => Set((sto.lookupValue(adr), Delta.empty(sto))))(inject)
        // CTX STUFF
        def getCtx =
            (_, _, sto, ctx) => Set((ctx, Delta.empty(sto)))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, sto, ctx) => blk(anl, env, sto, f(ctx))
        // ENV STUFF
        def getEnv =
            (_, env, sto, _) => Set((env, Delta.empty(sto)))
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, sto, ctx) => blk(anl, f(env), sto, ctx)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, sto, ctx) => anl.call(CallComponent(lam, env, sto, ctx))

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new SchemeLocalIntraAnalysis(cmp)
    class SchemeLocalIntraAnalysis(cmp: Cmp) extends IntraAnalysis(cmp):
        intra =>

        // local state
        var results = inter.results

        def call(cmp: Cmp): Set[(Val, Dlt)] =
            spawn(cmp)
            register(ResultDependency(cmp))
            results.getOrElse(cmp, Set.empty)

        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            val res = eval(cmp.exp)(this, cmp.env, cmp.sto, cmp.ctx)
            val rgc = res.map((v,d) => (v, d.collect(lattice.refs(v) ++ d.updates)))
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