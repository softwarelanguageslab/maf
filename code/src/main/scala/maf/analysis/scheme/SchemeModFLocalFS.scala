package maf.analysis.scheme

import maf.analysis.*
import maf.domain.*
import maf.domain.lattice.*
import maf.domain.scheme.*
import maf.semantics.scheme.*
import maf.syntax.Position
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.Preluded
import maf.util.*
import maf.util.Monad.MonadSyntaxOps

import scala.annotation.unused

abstract class SchemeModFLocalFS(gc: Boolean = true) extends Analysis[SchemeExp[Named] & Preluded] with SchemeSemantics:
    inter: SchemeDomain with SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount
    type Res = (Val, Dlt, Set[Adr], Set[Adr])
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
            .map { case (name, p) =>
                (name, PrmAddr(name), lattice.primitive(p.name))
            }

    //
    // COMPONENTS
    //

    sealed trait Component:
        def exp: Exp
        def env: Env
        def ctx: Ctx
    case class MainComponent(prg: SchemeExp[Named] & Preluded) extends Component with SmartHash:
        val exp = prg
        val env = BasicEnvironment(initialBds(prg).map(p => (p._1, p._2)).toMap)
        val ctx = initialCtx
        // the store that the program starts from
        val sto: Sto = LocalStore.from(initialBds(prg).map(p => (p._2, p._3)))
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, ctx: Ctx) extends Component:
        def exp = lam.bodyExp
        override def toString = s"${lam.lambdaName} [$ctx]"

    def initialComponent(prg: SchemeExp[Named] & Preluded): Component = MainComponent(prg)

    //
    // STATE = RESULTS + STORE
    //

    // the dependencies of this analysis
    sealed trait Dependency extends Analysis.Dependency

    // the result of a component, if it returns: its value, how it changes the store, and the addresses that it updates and allocates
    case class ResultDependency(cmp: Cmp) extends Dependency:
        type Value = Option[Res]
        given lattice: Lattice[Option[Res]] = resultLattice

    // the store of a call component, which its callers extend
    case class StoreDependency(cll: Cll) extends Dependency:
        type Value = Sto
        given lattice: Lattice[Sto] = storeLattice

    lazy val resultLattice: Lattice[Option[Res]] = new Lattice[Option[Res]]:
        def show(res: Option[Res]): String = res.toString
        def bottom: Option[Res] = None
        def top = throw LatticeTopUndefined
        def join(x: Option[Res], y: => Option[Res]): Option[Res] = (x, y) match
            case (None, res) => res
            case (res, None) => res
            case (Some((v1, d1, u1, a1)), Some((v2, d2, u2, a2))) => Some((lattice.join(v1, v2), d1.join(d2), u1 ++ u2, a1 ++ a2))
        def subsumes(x: Option[Res], y: => Option[Res]): Boolean = (x, y) match
            case (_, None) => true
            case (None, _) => false
            case (Some((v1, d1, u1, a1)), Some((v2, d2, u2, a2))) => lattice.subsumes(v1, v2) && d1.subsumes(d2) && u2.subsetOf(u1) && a2.subsetOf(a1)
        def eql[B: BoolLattice](x: Option[Res], y: Option[Res]): B = ???
        def level(res: Option[Res]): Int = res.size

    lazy val storeLattice: Lattice[Sto] = new Lattice[Sto]:
        def show(sto: Sto): String = sto.toString
        def bottom: Sto = LocalStore.empty
        def top = throw LatticeTopUndefined
        def join(x: Sto, y: => Sto): Sto = x.join(y)
        def subsumes(x: Sto, y: => Sto): Boolean = x.subsumes(y)
        def eql[B: BoolLattice](x: Sto, y: Sto): B = ???
        def level(sto: Sto): Int = sto.content.size


    //
    // STORE STUFF
    //

    def extendV(@unused anl: Anl, sto: Sto, adr: Adr, vlu: Val): Dlt = sto.extend(adr, vlu)
    def updateV(@unused anl: Anl, sto: Sto, adr: Adr, vlu: Val): Dlt = sto.update(adr, vlu)
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

    override protected def nontail[X](ignore: => Set[Adr]) (blk: => A[X]) =  
        if gc then 
            (anl, env, sto, ctx) =>
                blk(anl, env, sto, ctx).map { (v, d, u, a) =>
                    (v, sto.replay(d, a), u, a)
                }
        else blk

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
            (anl, _, sto, _) => Some((), extendV(anl, sto, adr, vlu), Set.empty, Set(adr))
        def updateSto(adr: Adr, vlu: Val) =
            (anl, _, sto, _) => Some((), updateV(anl, sto, adr, vlu), Set(adr), Set.empty)
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
            (anl, env, sto, ctx) =>
                val cll = CallComponent(lam, env, ctx)
                // add bindings to its store
                anl.set(StoreDependency(cll), sto)
                anl.spawn(cll)
                anl.get(ResultDependency(cll))

    //
    // THE ANALYSIS OF A COMPONENT
    //

    def computeOneCounts(solver: Solver): (Int, Int) =
        val stores = solver.components.toList.map {
            case main: MainComponent => main.sto
            case cll: Cll            => solver.value(StoreDependency(cll))
        }
        stores.foldLeft((0,0)) { 
            case (acc, store) =>
                store.content.foldLeft(acc) {
                    case (acc2@(countOne, total), (adr, (_ , count))) =>
                        if shouldCount(adr) then 
                            count match
                                case CountZero => throw new Exception("Should not happen ...")
                                case CountOne => (countOne + 1, total + 1)
                                case CountInf => (countOne,     total + 1)
                        else 
                            acc2
                }
        }

    def computeOneCountsPercentage(solver: Solver): Option[Double] =
        val (one, total) = computeOneCounts(solver)
        if total == 0 then
            None
        else 
            Some(one.toDouble / total.toDouble)

    def analyzeComponent(cmp: Component, anl: Anl): Unit =
        // get the (widened) store
        val sto = cmp match
            case main: MainComponent => main.sto
            case cll: Cll            => anl.get(StoreDependency(cll))
        // compute the result
        anl.set(ResultDependency(cmp), eval(cmp.exp)(anl, cmp.env, sto, cmp.ctx))


trait SchemeModFLocalFSAnalysisResults extends SchemeModFLocalFS with AnalysisResults[SchemeExp[Any]]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    // the values that the analysis writes to the variables and pointers of a position, which no component reads
    case class PositionDependency(pos: Option[Position]) extends Dependency:
        type Value = Set[Val]
        given lattice: Lattice[Set[Val]] = positionLattice
    lazy val positionLattice: Lattice[Set[Val]] = Lattice.setLattice

    def resultsPerPos(solver: Solver) = solver.dependencies.collect { case dep: PositionDependency => dep.pos -> solver.value(dep) }.toMap

    private def record(anl: Anl, adr: Adr, vlu: Val): Unit = adr match
        case _: VarAddr[_] | _: PtrAddr[_] => anl.set(PositionDependency(adr.pos), Set(vlu))
        case _                             => ()

    override def extendV(anl: Anl, sto: Sto, adr: Adr, vlu: Val) =
        record(anl, adr, vlu)
        super.extendV(anl, sto, adr, vlu)

    override def updateV(anl: Anl, sto: Sto, adr: Adr, vlu: Val) =
        record(anl, adr, vlu)
        super.updateV(anl, sto, adr, vlu)
