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

abstract class SchemeModFLocal(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
    inter: SchemeDomain with SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]

    //
    // INITIALISATION
    //

    lazy val initialExp: Exp = program
    lazy val initialEnv: Env = BasicEnvironment(initialBds.map(p => (p._1, p._2)).toMap)
    lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))(shouldCount)

    private def shouldCount(adr: Adr): Boolean = adr match
        case _: PtrAddr[_] => true
        case _             => false

    private lazy val initialBds: Iterable[(String, Adr, Val)] =
      primitives.allPrimitives.view
        .filterKeys(initialExp.fv)
        .map { case (name, p) =>
          (name, PrmAddr(name), lattice.primitive(p.name))
        }

    //
    // CALLBACKS (can be overriden)
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): sto.DeltaStore = sto.deltaStore.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): sto.DeltaStore = sto.deltaStore.update(adr, vlu)

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
    case class CallComponent(clo: Clo, ctx: Ctx, sto: Sto) extends Component:
        val (lam, env) = clo
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

    def withRestrictedStore[X](rs: Set[Adr])(blk: A[X]): A[X] =
      (res, ctx, env, sto) => {
        val (rss, cps, rds, wds) = blk(res, ctx, env, sto.collect(rs))
        (rss.map { case (x, d) => (x, sto.replay(d)) }, cps, rds, wds)
      }

    override protected def applyClosure(cll: Cll, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
      withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
        super.applyClosure(cll, lam, ags, fvs)
      }

    //
    // ANALYSISM MONAD
    //

    type A[X] = (res: Res, ctx: Ctx, env: Env, sto: Sto) => (Set[(X, sto.DeltaStore)], Set[Cmp], Set[Dep], Set[Dep])

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
          (_, _, _, sto) => (Set((x, sto.deltaStore)), Set(), Set(), Set())
        def map[X, Y](m: A[X])(f: X => Y) =
          (res, ctx, env, sto) => {
            val (rss, cps, rds, wds) = m(res, ctx, env, sto)
            (rss.map(res => (f(res._1), res._2)), cps, rds, wds)
          }
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
          (res, ctx, env, sto) =>
              val (rs1, cs1, rd1, wd1) = m(res, ctx, env, sto)
              rs1.foldLeft((Set[(Y, sto.DeltaStore)](), cs1, rd1, wd1)) { case (acc, (x, d0)) =>
                val (rs2, cs2, rd2, wd2) = f(x)(res, ctx, env, sto.integrate(d0))
                val rs3 = rs2.map { case (x, d1) => (x, sto.compose(d1, d0)) }
                (acc._1 ++ rs3, acc._2 ++ cs2, acc._3 ++ rd2, acc._4 ++ wd2)
              }
        // MONADJOIN
        def mbottom[X] =
          (_, _, _, _) => (Set(), Set(), Set(), Set())
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
          (res, ctx, env, sto) => {
            val (rs1, cs1, rd1, wd1) = x(res, ctx, env, sto)
            val (rs2, cs2, rd2, wd2) = y(res, ctx, env, sto)
            (rs1 ++ rs2, cs1 ++ cs2, rd1 ++ rd2, wd1 ++ wd2)
          }
        // MONADERROR
        def fail[X](err: Error) =
          mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
          (_, _, _, sto) => (Set((sto.addrEq, sto.deltaStore)), Set(), Set(), Set())
        def extendSto(adr: Adr, vlu: Val) =
          (_, _, env, sto) => (Set(((), extendV(sto, adr, vlu))), Set(), Set(), Set())
        def updateSto(adr: Adr, vlu: Val) =
          (_, _, env, sto) => (Set(((), updateV(sto, adr, vlu))), Set(), Set(), Set())
        def lookupSto(adr: Adr) =
          (_, _, _, sto) => (Set((sto(adr), sto.deltaStore)), Set(), Set(), Set())
        // CTX STUFF
        def getCtx =
          (_, ctx, _, sto) => (Set((ctx, sto.deltaStore)), Set(), Set(), Set())
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
          (res, ctx, env, sto) => blk(res, f(ctx), env, sto)
        // ENV STUFF
        def getEnv =
          (_, _, env, sto) => (Set((env, sto.deltaStore)), Set(), Set(), Set())
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
          (res, ctx, env, sto) => blk(res, ctx, f(env), sto)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
          (res, ctx, env, sto) => {
            val cmp = CallComponent((lam, env), ctx, sto)
            val rss = res.getOrElse(cmp, Set.empty).asInstanceOf[Set[(Val, sto.DeltaStore)]]
            (rss, Set(cmp), Set(ResultDependency(cmp)), Set())
          }

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) { intra =>
      var results = inter.results

      def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val (res, cps, rds, wds) = eval(cmp.exp)(results, cmp.ctx, cmp.env, cmp.sto)
          val rgc = res.map { case (x, d) => (x, d.collect(lattice.refs(x) ++ d.updates)) }
          val old = results.get(cmp)
          if rgc != old then
              results = results + (cmp -> rgc)
              trigger(ResultDependency(cmp))
          cps.foreach(spawn)
          rds.foreach(register)
          assert(wds.isEmpty)
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
