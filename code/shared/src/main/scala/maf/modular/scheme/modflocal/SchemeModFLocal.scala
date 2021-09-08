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
    lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))

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
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): sto.Delta = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): sto.Delta = sto.update(adr, vlu)

    def eqA(sto: Sto): MaybeEq[Adr] = new MaybeEq[Adr] {
        def apply[B: BoolLattice](a1: Adr, a2: Adr): B = 
            if a1 == a2 then
                sto.content.get(a1) match 
                    case Some((_, CountOne)) => BoolLattice[B].inject(true)
                    case _ => BoolLattice[B].top
            else BoolLattice[B].inject(false)
    }

    //
    // GC'ing
    //

    object StoreGC extends AbstractGarbageCollector[Sto, Adr]:
        def fresh(cur: Sto) = LocalStore.empty
        def move(addr: Adr, from: Sto, to: Sto) = 
            from.content.get(addr) match
                case None => (to, Set.empty)
                case Some(s@(v,_)) => (LocalStore(to.content + (addr -> s)), lattice.refs(v))

    object DeltaGC extends AbstractGarbageCollector[Sto#Delta, Adr]:
        def fresh(cur: Sto#Delta) = cur.copy(delta = Map.empty)
        def move(addr: Adr, from: Sto#Delta, to: Sto#Delta): (Sto#Delta, Set[Adr]) = 
            from.delta.get(addr).orElse(from.parent.content.get(addr)) match
                case None => (to, Set.empty)
                case Some((v, _)) if !from.delta.contains(addr) => (to, lattice.refs(v))
                case Some(s@(v,_)) => (to.copy(delta = to.delta + (addr -> s)), lattice.refs(v))

    def withRestrictedStore[X](rs: Set[Adr])(blk: A[X]): A[X] =
        (res, ctx, env, sto) =>
            val gcs = StoreGC.collect(sto, rs)
            val (rss, cps) = blk(res, ctx, env, gcs)
            (rss.map((x, d) => (x, sto.replay(gcs, d))), cps)

    override protected def applyClosure(cll: Cll, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
        withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
            super.applyClosure(cll, lam, ags, fvs)
        }

    //
    // ANALYSISM MONAD
    //

    type A[X] = (res: Res, ctx: Ctx, env: Env, sto: Sto) => (Set[(X, sto.Delta)], Set[Cmp])

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
          (_, _, _, sto) => (Set((x, sto.emptyDelta)), Set())
        def map[X, Y](m: A[X])(f: X => Y) =
          (res, ctx, env, sto) => {
            val (rss, cps) = m(res, ctx, env, sto)
            (rss.map((x, d) => (f(x), d)), cps)
          }
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
            (res, ctx, env, sto0) =>
                val (rs1, cs1) = m(res, ctx, env, sto0)
                rs1.foldLeft((Set.empty[(Y, sto0.Delta)], cs1)) { case (acc, (x, d0)) =>
                    val sto1 = sto0.integrate(d0)
                    val (rs2, cs2) = f(x)(res, ctx, env, sto1)
                    val rs3 = rs2.map((x, d1) => (x, sto0.compose(d1, d0)))
                    (acc._1 ++ rs3, acc._2 ++ cs2)
                }
        // MONADJOIN
        def mbottom[X] =
          (_, _, _, _) => (Set.empty, Set.empty)
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
          (res, ctx, env, sto) => {
            val (rs1, cs1) = x(res, ctx, env, sto)
            val (rs2, cs2) = y(res, ctx, env, sto)
            (rs1 ++ rs2, cs1 ++ cs2)
          }
        // MONADERROR
        def fail[X](err: Error) =
          mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
          (_, _, _, sto) => (Set((eqA(sto), sto.emptyDelta)), Set())
        def extendSto(adr: Adr, vlu: Val) =
          (_, _, env, sto) => (Set(((), extendV(sto, adr, vlu))), Set())
        def updateSto(adr: Adr, vlu: Val) =
          (_, _, env, sto) => (Set(((), updateV(sto, adr, vlu))), Set())
        def lookupSto(adr: Adr) =
          (_, _, _, sto) => (Set((sto(adr), sto.emptyDelta)), Set())
        // CTX STUFF
        def getCtx =
          (_, ctx, _, sto) => (Set((ctx, sto.emptyDelta)), Set())
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
          (res, ctx, env, sto) => blk(res, f(ctx), env, sto)
        // ENV STUFF
        def getEnv =
          (_, _, env, sto) => (Set((env, sto.emptyDelta)), Set())
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
          (res, ctx, env, sto) => blk(res, ctx, f(env), sto)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
          (res, ctx, env, sto) => {
            val cmp = CallComponent(lam, env, ctx, sto)
            val rss = res.getOrElse(cmp, Set.empty).asInstanceOf[Set[(Val, sto.Delta)]]
            (rss, Set(cmp))
          }

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) { intra =>
      var results = inter.results

      def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val (res, cps) = eval(cmp.exp)(results, cmp.ctx, cmp.env, cmp.sto)
          val rgc = res.map { case (x, d) => (x, DeltaGC.collect(d, lattice.refs(x) ++ d.updates)) }
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
