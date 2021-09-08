package maf.modular.scheme.modflocal

import maf.modular._
import maf.modular.scheme._
import maf.core._
import maf.core.CountingStoreOps
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.util.benchmarks.Timeout
import maf.language.CScheme._
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.LatticeWithAddrs

abstract class SchemeModFLocal(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
    inter: SchemeDomain with SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Dep = Dependency
    type Sto = CountingStore[Adr, Val]

    //
    // INITIALISATION
    //

    lazy val initialExp: Exp = program
    lazy val initialEnv: Env = BasicEnvironment(initialBds.map(p => (p._1, p._2)).toMap)
    lazy val initialSto: Sto = CountingStoreOps[Adr,Val].from(initialBds.map(p => (p._2, p._3)))

    private def shouldCount(adr: Adr): Boolean = adr match
        case _: PtrAddr[_] => true
        case _             => false

    given p: (Adr => Boolean) = shouldCount

    private lazy val initialBds: Iterable[(String, Adr, Val)] =
      primitives.allPrimitives.view
        .filterKeys(initialExp.fv)
        .map { case (name, p) =>
          (name, PrmAddr(name), lattice.primitive(p.name))
        }

    //
    // CALLBACKS (can be overriden)
    //

    def extendV(lcl: Sto, adr: Adr, vlu: Val)(using sto: StoreOps[Sto, Adr, Val]): lcl.Delta = sto.extend(lcl, adr, vlu)
    def updateV(lcl: Sto, adr: Adr, vlu: Val)(using sto: StoreOps[Sto, Adr, Val]): lcl.Delta = sto.update(lcl, adr, vlu)

    //
    // COMPONENTS
    //

    sealed trait Component extends Serializable:
        def exp: Exp
        def env: Env
        val sto: Sto
        val ctx: Ctx
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val sto = initialSto
        val ctx = initialCtx
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, ctx: Ctx, sto: Sto) extends Component:
        def exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName} [$ctx] [$sto]"

    def initialComponent: Component = MainComponent
    def expr(cmp: Component): Exp = cmp.exp

    //
    // RESULTS
    //

    type Res = Map[Cmp, Any] // Res is actually (cmp: Cmp) => Set[(Val, cmp.sto.DeltaStore)]

    var results: Res = Map.empty
    case class ResultDependency(cmp: Cmp) extends Dependency

    //
    // GC'ing
    //

    def withRestrictedStore[X](rs: Set[Adr])(blk: A[X])(using sto: StoreOps[Sto, Adr, Val]): A[X] =
      (res, ctx, env, lcl) => {
        val (rss, cps) = blk(res, ctx, env, lcl) //TODO: sto.collect(rs)
        (rss.map((x, d) => (x, d)), cps)        //TODO: sto.replay(lcl, d)
      }

    override protected def applyClosure(cll: Cll, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
      withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
        super.applyClosure(cll, lam, ags, fvs)
      }

    //
    // ANALYSISM MONAD
    //

    type A[X] = (res: Res, ctx: Ctx, env: Env, lcl: Sto) => (Set[(X, lcl.Delta)], Set[Cmp])
    lazy val sto = CountingStoreOps[Adr, Val]  

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
          (_, _, _, lcl) => (Set((x, sto.delta(lcl))), Set())
        def map[X, Y](m: A[X])(f: X => Y) =
          (res, ctx, env, lcl) => {
            val (rss, cps) = m(res, ctx, env, lcl)
            (rss.map((x, d) => (f(x), d)), cps)
          }
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
          (res, ctx, env, lcl0) =>
              val (rs1, cs1) = m(res, ctx, env, lcl0)
              rs1.foldLeft((Set[(Y, lcl0.Delta)](), cs1)) { case (acc, (x, d0)) =>
                val lcl1 = sto.integrate(lcl0, d0)
                val (rs2, cs2) = f(x)(res, ctx, env, lcl1)
                val rs3 = rs2.map((x, d1) => (x, sto.compose(lcl1, d1)(lcl0, d0): lcl0.Delta))
                (acc._1 ++ rs3, acc._2 ++ cs2)
              }
        // MONADJOIN
        def mbottom[X] =
          (_, _, _, _) => (Set.empty, Set.empty)
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
          (res, ctx, env, lcl) => {
            val (rs1, cs1) = x(res, ctx, env, lcl)
            val (rs2, cs2) = y(res, ctx, env, lcl)
            (rs1 ++ rs2, cs1 ++ cs2)
          }
        // MONADERROR
        def fail[X](err: Error) =
          mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
          (_, _, _, lcl) => (Set((sto.addrEq(lcl), sto.delta(lcl))), Set())
        def extendSto(adr: Adr, vlu: Val) =
          (_, _, env, lcl) => (Set(((), extendV(lcl, adr, vlu))), Set())
        def updateSto(adr: Adr, vlu: Val) =
          (_, _, env, lcl) => (Set(((), updateV(lcl, adr, vlu))), Set())
        def lookupSto(adr: Adr) =
          (_, _, _, lcl) => (Set((lcl(adr), sto.delta(lcl))), Set())
        // CTX STUFF
        def getCtx =
          (_, ctx, _, lcl) => (Set((ctx, sto.delta(lcl))), Set())
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
          (res, ctx, env, lcl) => blk(res, f(ctx), env, lcl)
        // ENV STUFF
        def getEnv =
          (_, _, env, lcl) => (Set((env, sto.delta(lcl))), Set())
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
          (res, ctx, env, lcl) => blk(res, ctx, f(env), lcl)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
          (res, ctx, env, lcl) => {
            val cmp = CallComponent(lam, env, ctx, lcl)
            val rss = res.getOrElse(cmp, Set.empty).asInstanceOf[Set[(Val, lcl.Delta)]]
            (rss, Set(cmp))
          }

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) { intra =>

      var results = inter.results

      def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val (res, cps) = eval(cmp.exp)(results, cmp.ctx, cmp.env, cmp.sto)
          val rgc = res.map { case (x, d) => (x, d) }// d.collect(lattice.refs(x) ++ d.updates)) }
          val old = results.get(cmp)
          if rgc != old then
              results = results + (cmp -> rgc)
              trigger(ResultDependency(cmp))
          cps.foreach(spawn)
          cps.foreach(cmp => register(ResultDependency(cmp)))
      override def doWrite(dep: Dependency): Boolean = dep match
          case ResultDependency(cmp) =>
            val old = inter.results.getOrElse(cmp, Set.empty)
            val cur = intra.results.getOrElse(cmp, Set.empty)
            if old != cur then
                inter.results += cmp -> cur
                true
            else false
          case _ => super.doWrite(dep)
    }
