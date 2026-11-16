package maf.analysis.scheme

import maf.analysis.*
import maf.domain.*
import maf.domain.lattice.*
import maf.domain.scheme.*
import maf.fixpoint.*
import maf.semantics.scheme.*
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.Preluded
import maf.util.*
import maf.util.Monad.MonadSyntaxOps

abstract class SchemeModFADI(prg: SchemeExp[Named] & Preluded) extends ModAnalysis[SchemeExp[Named]](prg) with SchemeSemantics:
    inter: SchemeDomain & SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = Store.CountingStore[Adr, Val]
    type Cnt = AbstractCount
    type Anl = AnalysisState

    given GC[Sto, Adr] = GC.storeStopAndCopyGC
    given store: Store[Sto, Adr, Val] = Store.countingInstance
    given shouldCount: (Adr => Boolean) =
        case _: PtrAddr[_] => true
        case _             => false

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

    //
    // COMPONENTS
    //

    sealed trait Component:
        val exp: Exp
        val env: Env
        val sto: Sto
        val ctx: Ctx
        val krs: Set[Adr]
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        val sto = initialSto
        val krs = Set.empty
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, sto: Sto, ctx: Ctx, krs: Set[Adr]) extends Component:
        val exp = lam.bodyExp
        override def toString = s"${lam.lambdaName}@${lam.pos.show} [$ctx] [<store>]"

    def initialComponent: Cmp = MainComponent

    //
    // RESULTS
    //

    // for every path through a component: its value and the resulting store
    type Res = (Val, Sto)

    case class ResultDependency(cmp: Component) extends Dependency:
        type Value = Set[Res]
        given lattice: Lattice[Set[Res]] = resultLattice

    // a set of results subsumes another one if each result of the latter is subsumed by a result of the former
    lazy val resultLattice: Lattice[Set[Res]] = Lattice.SubsumptionSetLattice[Res] { case ((v1, s1), (v2, s2)) =>
        lattice.subsumes(v1, v2) && s1.subsumes(s2)
    }

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): Sto = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): Sto = sto.update(adr, vlu)

    def eqA(sto: Sto): MaybeEq[Adr] = sto.addrEq

    def withRestrictedStore(rs: Set[Adr])(blk: A[Val]): A[Val] =
        (anl, env, sto, ctx, krs) =>
            blk(anl, env, sto.collect(rs ++ krs), ctx, krs).map { 
                (v, rsto) =>
                    (v, rsto.collect(lattice.refs(v) ++ krs))
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

    override protected def nontail[X](add: => Set[Adr])(blk: => A[X]) =  
        (anl, env, sto, ctx, krs) => blk(anl, env, sto, ctx, krs ++ add)
    //
    // ANALYSISM MONAD
    //

    type A[X] = (anl: Anl, env: Env, sto: Sto, ctx: Ctx, krs: Set[Adr]) => Set[(X, Sto)]

    protected def analysisM: AnalysisM[A] = new AnalysisM[A]:
        // MONAD
        def unit[X](x: X) =
            (_, _, sto, _, _) => Set((x, sto))
        def map[X, Y](m: A[X])(f: X => Y) =
            (anl, env, sto, ctx, rs) => m(anl, env, sto, ctx, rs).map((x, s) => (f(x), s))
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (anl, env, sto, ctx, krs) =>
                for
                    (x0, sto1) <- m(anl, env, sto, ctx, krs)
                    (x1, sto2) <- f(x0)(anl, env, sto1, ctx, krs)
                yield (x1, sto2)
        // MONADJOIN
        def mbottom[X] =
            (_, _, _, _, _) => Set.empty
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
            (anl, env, sto, ctx, krs) => x(anl, env, sto, ctx, krs) ++ y(anl, env, sto, ctx, krs)
        // MONADERROR
        def fail[X](err: Error) =
            mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
            (_, _, sto, _, _) => Set((eqA(sto), sto))
        def extendSto(adr: Adr, vlu: Val) =
            (_, _, sto, _, _) => Set(((), extendV(sto, adr, vlu)))
        def updateSto(adr: Adr, vlu: Val) =
            (_, _, sto, _, _) => Set(((), updateV(sto, adr, vlu)))
        def lookupSto(adr: Adr) =
            flatMap((_, _, sto, _, _) => Set((sto.lookup(adr), sto)))(inject)
        // CTX STUFF
        def getCtx =
            (_, _, sto, ctx, _) => Set((ctx, sto))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            (anl, env, sto, ctx, krs) => blk(anl, env, sto, f(ctx), krs)
        // ENV STUFF
        def getEnv =
            (_, env, sto, _, _) => Set((env, sto))
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            (anl, env, sto, ctx, krs) => blk(anl, f(env), sto, ctx, krs)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
            (anl, env, sto, ctx, krs) =>
                val cll = CallComponent(lam, env, sto, ctx, krs)
                anl.spawn(cll)
                anl.get(ResultDependency(cll))

    //
    // THE ANALYSIS OF A COMPONENT
    //

    def analyzeComponent(cmp: Component, anl: Anl): Unit =
        anl.set(ResultDependency(cmp), eval(cmp.exp)(anl, cmp.env, cmp.sto, cmp.ctx, cmp.krs))


trait SchemeModFADIAnalysisResults extends SchemeModFADI with AnalysisResults[SchemeExp[Any]]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    var resultsPerPos = Map.empty.withDefaultValue(Set.empty)

    override def extendV(sto: Sto, adr: Adr, vlu: Val) =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
                resultsPerPos += adr.pos -> (resultsPerPos(adr.pos) + vlu)
            case _ => ()
        super.extendV(sto, adr, vlu)

    override def updateV(sto: Sto, adr: Adr, vlu: Val) =
        adr match
            case _: VarAddr[_] | _: PtrAddr[_] =>
                resultsPerPos += adr.pos -> (resultsPerPos(adr.pos) + vlu)
            case _ => ()
        super.updateV(sto, adr, vlu)

// a standard instance

class SchemeModFADIAnalysis(prg: SchemeExp[Named] & Preluded, k: Int)
    extends SchemeModFADI(prg)
    with SchemeConstantPropagationDomain
    with SchemeModFLocalCallSiteSensitivity(k)
    with maf.fixpoint.FIFOWorklistAlgorithm
        //override def run(t: maf.util.Timeout.T) = 
        //    super.run(t)
        //    println(value(ResultDependency(MainComponent)))
