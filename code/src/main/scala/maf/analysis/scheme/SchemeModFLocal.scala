package maf.analysis.scheme

import maf.analysis.*
import maf.domain.*
import maf.domain.lattice.*
import maf.domain.scheme.*
import maf.fixpoint.Solver
import maf.semantics.scheme.*
import maf.syntax.Position
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.Preluded
import maf.util.*
import maf.util.Monad.MonadSyntaxOps

import scala.annotation.unused

abstract class SchemeModFLocal extends Analysis[SchemeExp[Named] & Preluded] with SchemeSemantics:
    inter: SchemeDomain & SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount
    type Anl = AnalysisState

    //
    // INITIALISATION
    //

    given shouldCount: (Adr => Boolean) =
        case _: PtrAddr[_] => true
        case _             => false

    // the bindings of the primitives that the program `prg` uses, which its analysis starts from
    private def initialBds(prg: Exp): Iterable[(String, Adr, Val)] =
        primitives.allPrimitives.view
            .filterKeys(prg.fv)
            .map { case (name, p) => (name, PrmAddr(name), lattice.primitive(p.name)) }

    //
    // COMPONENTS
    //

    sealed trait Component:
        val exp: Exp
        val env: Env
        val sto: Sto
        val ctx: Ctx
    case class MainComponent(prg: SchemeExp[Named] & Preluded) extends Component with SmartHash:
        val exp = prg
        val env = Environment(initialBds(prg).map(p => (p._1, p._2)))
        val ctx = initialCtx
        val sto = LocalStore.from(initialBds(prg).map(p => (p._2, p._3)))
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, sto: Sto, ctx: Ctx) extends Component:
        val exp = lam.bodyExp
        override def toString = s"${lam.lambdaName}@${lam.pos.show} [$ctx] [${sto.content.hc}]"

    def initialComponent(prg: SchemeExp[Named] & Preluded): Cmp = MainComponent(prg)

    //
    // RESULTS
    //

    // for every path through a component: its value, how it changes the store, and the addresses that it updates and allocates
    type Res = (Val, Dlt, Set[Adr], Set[Adr])

    // the dependencies of this analysis
    sealed trait Dependency extends Analysis.Dependency

    case class ResultDependency(cmp: Component) extends Dependency:
        type Value = Set[Res]
        given lattice: Lattice[Set[Res]] = resultLattice

    // a set of results subsumes another one if each result of the latter is subsumed by a result of the former
    lazy val resultLattice: Lattice[Set[Res]] = Lattice.SubsumptionSetLattice[Res] { case ((v1, d1, u1, a1), (v2, d2, u2, a2)) =>
        lattice.subsumes(v1, v2) && d1.subsumes(d2) && u2.subsetOf(u1) && a2.subsetOf(a1)
    }

    //
    // STORE STUFF
    //

    def extendV(@unused anl: Anl, sto: Sto, adr: Adr, vlu: Val): Dlt = sto.extend(adr, vlu)
    def updateV(@unused anl: Anl, sto: Sto, adr: Adr, vlu: Val): Dlt = sto.update(adr, vlu)

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
                val gcd = DeltaGC(gcs).collect(d, lattice.refs(v) ++ gcu)
                val gca = a.filter(gcd.delta.contains)
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

    override protected def nontail[X](ignore: => Set[Adr])(blk: => A[X]) =  
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
            (_, _, _, _) => Set((x, Delta.emptyDelta[Adr,Val], Set.empty, Set.empty))
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
            (_, _, sto, _) => Set((eqA(sto), Delta.emptyDelta, Set.empty, Set.empty))
        def extendSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Set(((), extendV(anl, sto, adr, vlu), Set.empty, Set(adr)))
        def updateSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Set(((), updateV(anl, sto, adr, vlu), Set(adr), Set.empty))
        def lookupSto(adr: Adr) =
            flatMap((_, _, sto, _) => Set((sto.lookupValue(adr), Delta.emptyDelta, Set.empty, Set.empty)))(inject)
        // CTX STUFF
        def getCtx =
            (_, _, _, ctx) => Set((ctx, Delta.emptyDelta, Set.empty, Set.empty))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, sto, ctx) => blk(anl, env, sto, f(ctx))
        // ENV STUFF
        def getEnv =
            (_, env, _, _) => Set((env, Delta.emptyDelta, Set.empty, Set.empty))
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, sto, ctx) => blk(anl, f(env), sto, ctx)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, sto, ctx) =>
                val cll = CallComponent(lam, env, sto, ctx)
                anl.spawn(cll)
                anl.get(ResultDependency(cll))

    //
    // THE ANALYSIS OF A COMPONENT
    //

    def analyzeComponent(cmp: Component, anl: Anl): Unit =
        anl.set(ResultDependency(cmp), eval(cmp.exp)(anl, cmp.env, cmp.sto, cmp.ctx))


trait SchemeModFLocalAnalysisResults extends SchemeModFLocal with AnalysisResults[SchemeExp[Any]]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    // the values that the analysis writes to the variables and pointers of a position, which no component reads
    case class PositionDependency(pos: Option[Position]) extends Dependency:
        type Value = Set[Val]
        given lattice: Lattice[Set[Val]] = positionLattice
    lazy val positionLattice: Lattice[Set[Val]] = Lattice.setLattice

    def resultsPerPos(solver: Solver[this.type]) = solver.dependencies.collect { case dep: PositionDependency => dep.pos -> solver.value(dep) }.toMap

    private def record(anl: Anl, adr: Adr, vlu: Val): Unit = adr match
        case _: VarAddr[_] | _: PtrAddr[_] => anl.set(PositionDependency(adr.pos), Set(vlu))
        case _                             => ()

    override def extendV(anl: Anl, sto: Sto, adr: Adr, vlu: Val) =
        record(anl, adr, vlu)
        super.extendV(anl, sto, adr, vlu)

    override def updateV(anl: Anl, sto: Sto, adr: Adr, vlu: Val) =
        record(anl, adr, vlu)
        super.updateV(anl, sto, adr, vlu)

// a standard instance

class SchemeDSSAnalysis(k: Int)
    extends SchemeModFLocal
    with SchemeConstantPropagationDomain
    with SchemeModFLocalCallSiteSensitivity(k)
