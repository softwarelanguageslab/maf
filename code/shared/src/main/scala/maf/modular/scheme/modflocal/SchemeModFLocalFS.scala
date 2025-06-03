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
import maf.util.datastructures.SmartMap
import maf.core.Monad.MonadSyntaxOps

abstract class SchemeModFLocalFS(prg: SchemeExp, gc: Boolean = true) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
    inter: SchemeDomain with SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount
    type Res = Map[Cmp, (Val, Dlt, Set[Adr], Set[Adr])]
    type Sts = Map[Cmp, Sto]
    type Anl = SchemeModFLocalFSIntraAnalysis

    //
    // INITIALISATION
    //

    lazy val initialExp: Exp = program
    lazy val initialEnv: Env = BasicEnvironment(initialBds.map(p => (p._1, p._2)).toMap)
    lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))

    given shouldCount: (Adr => Boolean) =
        case _: PtrAddr[_] => true
        case _             => false

    private lazy val initialBds: Iterable[(String, Adr, Val)] =
        primitives.allPrimitives.view
            .filterKeys(initialExp.fv)
            .map { case (name, p) =>
                (name, PrmAddr(name), lattice.primitive(p.name))
            }

    //
    // COMPONENTS
    //

    sealed trait Component extends Serializable:
        def exp: Exp
        def env: Env
        def ctx: Ctx
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, ctx: Ctx) extends Component:
        def exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName} [$ctx]"

    def initialComponent: Component = MainComponent
    def expr(cmp: Component): Exp = cmp.exp

    //
    // STATE = RESULTS + STORE
    //

    var results: Res = Map.empty
    case class ResultDependency(cmp: Cmp) extends Dependency

    var stores: Sts = Map.empty
    case class AddrDependencyFS(cmp: Cmp, adr: Adr) extends Dependency

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.update(adr, vlu)
    def eqA(sto: Sto): MaybeEq[Adr] = new MaybeEq[Adr]:
        def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
            if a1 == a2 then
                sto.content.get(a1) match
                    case Some((_, CountOne)) => BoolLattice[B].inject(true)
                    case _                   => BoolLattice[B].top
            else BoolLattice[B].inject(false)

    def withRestrictedStore(rs: => Set[Adr])(blk: A[Val]): A[Val] =
        if gc then
            (anl, env, sto, ctx) =>
                val gcs = LocalStoreGC().collect(sto, rs)
                blk(anl, env, sto, ctx).map { (vlu, dlt, upd, all) =>
                    val gcu = upd.filter(gcs.contains)
                    val gcd = DeltaGC(gcs).collect(dlt, lattice.refs(vlu) ++ gcu)
                    val gca = all.filter(gcd.delta.contains)
                    (vlu, gcd, gcu, gca)
                }
        else blk

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
        if gc then 
            (anl, env, sto, ctx) =>
                blk(anl, env, sto, ctx).map { (v, d, u, a) =>
                    (v, sto.replay(d, a), u, a)
                }
        else blk

    override def init() =
        super.init()
        stores += MainComponent -> initialSto

    //
    // ANALYSISM MONAD
    //

    type A[X] = (Anl, Env, Sto, Ctx) => Option[(X, Dlt, Set[Adr], Set[Adr])]

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
            (_, _, _, _) => Some((x, Delta.emptyDelta, Set.empty, Set.empty))
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
            (_, _, _, _) => None
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
            (anl, env, sto, ctx) =>
                (x(anl, env, sto, ctx), y(anl, env, sto, ctx)) match
                    case (None, yres)                   => yres
                    case (xres, None)                   => xres
                    case (Some((xv, xs, xu, xa)), Some(yv, ys, yu, ya)) => Some((Lattice[X].join(xv, yv), sto.join(xs, ys), xu ++ yu, xa ++ ya))
        // MONADERROR
        def fail[X](err: Error) =
            mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
            (_, _, sto, _) => Some((eqA(sto), Delta.emptyDelta, Set.empty, Set.empty))
        def extendSto(adr: Adr, vlu: Val) =
            (_, _, sto, _) => Some((), extendV(sto, adr, vlu), Set.empty, Set(adr))
        def updateSto(adr: Adr, vlu: Val) =
            (_, _, sto, _) => Some((), updateV(sto, adr, vlu), Set(adr), Set.empty)
        def lookupSto(adr: Adr) =
            (_, _, sto, _) => sto.getValue(adr).map((_, Delta.emptyDelta, Set.empty, Set.empty))
        // CTX STUFF
        def getCtx =
            (_, _, _, ctx) => Some((ctx, Delta.emptyDelta, Set.empty, Set.empty))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]) =
            (anl, env, sto, ctx) => blk(anl, env, sto, f(ctx))
        // ENV STUFF
        def getEnv =
            (_, env, _, _) => Some((env, Delta.emptyDelta, Set.empty, Set.empty))
        def withEnv[X](f: Env => Env)(blk: A[X]) =
            (anl, env, sto, ctx) => blk(anl, f(env), sto, ctx)
        // CALL STUFF
        def call(lam: Lam) =
            (anl, env, sto, ctx) => anl.call(CallComponent(lam, env, ctx), sto)

    //
    // THE INTRA-ANALYSIS
    //

    var iterations = 0

    def intraAnalysis(cmp: Component) = new SchemeModFLocalFSIntraAnalysis(cmp)
    class SchemeModFLocalFSIntraAnalysis(cmp: Component) extends IntraAnalysis(cmp):
        intra =>
        
        // local state
        var results = inter.results
        var stores = inter.stores

        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            iterations = iterations + 1
            // get the (widened) store
            val sto = stores.getOrElse(cmp, LocalStore.empty)
            // register dependencies on all addresses
            sto.content.keys.foreach(adr => register(AddrDependencyFS(cmp, adr)))
            // GC the result
            val rgc = eval(cmp.exp)(this, cmp.env, sto, cmp.ctx)
            // update the result of this component
            val old = results.get(cmp)
            if (old != rgc) then
                results += cmp -> rgc.get
                trigger(ResultDependency(cmp))

        def call(cll: Cll, sto: Sto): Option[(Val, Dlt, Set[Adr], Set[Adr])] =
            // spawn the component
            spawn(cll)
            // add bindings to its store
            val stw = stores.getOrElse(cll, LocalStore.empty)
            val (upd, dty) = sto.content.foldLeft((stw, false)) { case (acc, (adr, (vlu, cnt))) =>
                acc._1.joinAt(adr, vlu, cnt) match
                    case None => acc
                    case Some(upd) =>
                        trigger(AddrDependencyFS(cll, adr))
                        (upd, true)
            }
            if dty then stores += cll -> upd
            register(ResultDependency(cll))
            results.get(cll)

        override def doWrite(dep: Dependency): Boolean = dep match
            case ResultDependency(cmp) =>
                val old = inter.results.get(cmp)
                val cur = intra.results(cmp)        // we are certain to have a result here!
                if !old.isDefined || old.get != cur then
                    inter.results += cmp -> cur
                    true
                else false
            case AddrDependencyFS(cmp, adr) =>
                val oldS = inter.stores.getOrElse(cmp, LocalStore.empty)
                val newS = intra.stores(cmp) // we are certain to have a store here!
                val (newV, newC) = newS.content(adr) // we are certain to have a binding here!
                oldS.joinAt(adr, newV, newC) match
                    case None => false
                    case Some(upd) =>
                        inter.stores += cmp -> upd
                        true
            case _ => super.doWrite(dep)


trait SchemeModFLocalFSAnalysisResults extends SchemeModFLocalFS with AnalysisResults[SchemeExp]:
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