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
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount

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
        val ctx: Ctx
        val sto: Sto
    case object MainComponent extends Component:
        val exp = initialExp
        val env = initialEnv
        val ctx = initialCtx
        val sto = LocalStore.empty
        override def toString = "main"
    case class CallComponent(lam: Lam, env: Env, ctx: Ctx, sto: Sto) extends Component:
        def exp = SchemeBody(lam.body)
        override def toString = s"${lam.lambdaName} [$ctx] [$sto]"

    def initialComponent: Component = MainComponent
    def expr(cmp: Component): Exp = cmp.exp

    //
    // RESULTS
    //

    type Res = Map[Cmp, (Set[(Val, Dlt)], Dlt)]

    var results: Res = Map.empty
    case class ResultDependency(cmp: Cmp) extends Dependency

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, stw: Sto, adr: Adr, vlu: Val): (Dlt, Dlt) =
      policy(adr) match
          case AddrPolicy.Local   => (sto.extend(adr, vlu), Delta.empty)
          case AddrPolicy.Widened => (Delta.empty, stw.extend(adr, vlu))
    def updateV(sto: Sto, stw: Sto, adr: Adr, vlu: Val): (Dlt, Dlt) =
      policy(adr) match
          case AddrPolicy.Local   => (sto.update(adr, vlu), Delta.empty)
          case AddrPolicy.Widened => (Delta.empty, stw.update(adr, vlu))
    def lookupV(sto: Sto, stw: Sto, adr: Adr): Val =
      policy(adr) match
          case AddrPolicy.Local   => sto(adr)
          case AddrPolicy.Widened => stw(adr)

    def eqA(sto: Sto): MaybeEq[Adr] = new MaybeEq[Adr] {
      def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
        if a1 == a2 then
            sto.content.get(a1) match
                case Some((_, CountOne)) => BoolLattice[B].inject(true)
                case _                   => BoolLattice[B].top
        else BoolLattice[B].inject(false)
    }

    //
    // GC'ing
    //

    trait AGC[X] extends AbstractGarbageCollector[(X, X), Adr]:
        def fresh1(cur: X): X
        def fresh(cur: (X, X)): (X, X) = (fresh1(cur._1), fresh1(cur._2))
        def move1(addr: Adr, from: X, to: X): (X, Set[Adr])
        def move(addr: Adr, from: (X, X), to: (X, X)): ((X, X), Set[Adr]) =
          policy(addr) match
              case AddrPolicy.Local =>
                val (lcl, rfs) = move1(addr, from._1, to._1)
                ((lcl, to._2), rfs)
              case AddrPolicy.Widened =>
                val (stw, rfs) = move1(addr, from._2, to._2)
                ((to._1, stw), rfs)

    object StoreGC extends AGC[Sto]:
        def fresh1(cur: Sto) = LocalStore.empty
        def move1(addr: Adr, from: Sto, to: Sto): (Sto, Set[Adr]) =
          from.content.get(addr) match
              case None             => (to, Set.empty)
              case Some(s @ (v, _)) => (LocalStore(to.content + (addr -> s)), lattice.refs(v))

    case object DeltaGC extends AGC[(Dlt, Sto)]:
        def fresh1(cur: (Dlt, Sto)) = (cur._1.copy(delta = Map.empty), cur._2) //TODO: this always carries over the set of updated addrs
        def move1(addr: Adr, from: (Dlt, Sto), to: (Dlt, Sto)): ((Dlt, Sto), Set[Adr]) =
          from._1.delta.get(addr).orElse(from._2.content.get(addr)) match
              case None                                          => (to, Set.empty)
              case Some((v, _)) if !from._1.delta.contains(addr) => (to, lattice.refs(v))
              case Some(s @ (v, _))                              => ((to._1.copy(delta = to._1.delta + (addr -> s)), to._2), lattice.refs(v))

    def withRestrictedStore(rs: Set[Adr])(blk: A[Val]): A[Val] =
      (res, ctx, env, sto, stw) =>
          val (gcs, gcw) = StoreGC.collect((sto, stw), rs)
          val (rs0, dw0, ef0) = blk(res, ctx, env, gcs, gcw)
          val (rs1, dw1) = collectDelta(gcs, gcw, rs0, dw0)
          (rs1.map((v, d) => (v, sto.replay(gcs, d))), stw.replay(gcw, dw1), ef0)

    def collectDelta(sto: Sto, stw: Sto, rss: Set[(Val, Dlt)], std: Dlt): (Set[(Val, Dlt)], Dlt) =
      rss.foldLeft((Set.empty[(Val, Dlt)], Delta.empty)) { case ((accR, accW), (v, d)) =>
        val ((gcd, _), (gdw, _)) = DeltaGC.collect(((d, sto), (std, stw)), lattice.refs(v) ++ d.updates ++ std.updates)
        (accR + ((v, gcd)), stw.join(accW, gdw))
      }

    override protected def applyClosure(cll: Cll, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
      withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
        super.applyClosure(cll, lam, ags, fvs)
      }

    //
    // STORE WIDENING
    //

    enum AddrPolicy:
        case Local
        case Widened

    var fixedPolicies: Map[Adr, AddrPolicy] = Map.empty
    def customPolicy(adr: Adr): AddrPolicy = AddrPolicy.Widened
    def policy(adr: Adr): AddrPolicy =
      fixedPolicies.get(adr) match
          case Some(ply) => ply
          case None      => customPolicy(adr)

    type Sts = Map[(Exp, Ctx), Sto]
    var stores: Sts = Map.empty

    def getStore(sts: Sts, exp: Exp, ctx: Ctx): Sto =
      sts.getOrElse((exp, ctx), LocalStore.empty)
    def setStore(sts: Sts, exp: Exp, ctx: Ctx, sto: Sto): Sts =
      sts + ((exp, ctx) -> sto)

    override def init() =
        super.init()
        stores = setStore(stores, MainComponent.exp, MainComponent.ctx, initialSto)
        initialBds.map(_._2).foreach(adr => fixedPolicies += adr -> AddrPolicy.Widened)

    case class WidenedAddrDependency(exp: Exp, ctx: Ctx, adr: Adr) extends Dependency

    //
    // EFFECTS
    //

    case class Eff(c: Set[Cmp], w: Set[(Exp, Ctx, Adr, Cnt, Val)]):
        def ++(other: Eff): Eff = Eff(c ++ other.c, w ++ other.w)
    object Eff:
        def empty = Eff(Set.empty, Set.empty)
        def call(cmp: Cmp) = Eff(Set(cmp), Set.empty)
        def write(exp: Exp, ctx: Ctx, adr: Adr, cnt: Cnt, vlu: Val) = Eff(Set.empty, Set((exp, ctx, adr, cnt, vlu)))
        def write(wrs: Iterable[(Exp, Ctx, Adr, Cnt, Val)]) = Eff(Set.empty, wrs.toSet)

    //
    // ANALYSISM MONAD
    //

    type A[X] = (Res, Ctx, Env, Sto, Sto) => (Set[(X, Dlt)], Dlt, Eff)

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
          (_, _, _, sto, stw) => (Set((x, Delta.empty)), Delta.empty, Eff.empty)
        def map[X, Y](m: A[X])(f: X => Y) =
          (res, ctx, env, sto, stw) =>
              val (rss, dlw, eff) = m(res, ctx, env, sto, stw)
              (rss.map((x, d) => (f(x), d)), dlw, eff)
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
          (res, ctx, env, sto0, stw0) =>
              val (rs1, dw1, ef1) = m(res, ctx, env, sto0, stw0)
              val stw1 = stw0.integrate(dw1)
              rs1.foldLeft((Set.empty[(Y, Dlt)], Delta.empty, ef1)) { case (acc, (x, d1)) =>
                val sto1 = sto0.integrate(d1)
                val (rs2, dw2, ef2) = f(x)(res, ctx, env, sto1, stw1)
                val rs3 = rs2.map((x, d2) => (x, sto0.compose(d2, d1)))
                (acc._1 ++ rs3, stw0.join(acc._2, stw0.compose(dw2, dw1)), acc._3 ++ ef2)
              }
        // MONADJOIN
        def mbottom[X] =
          (_, _, _, _, stw) => (Set.empty, Delta.empty, Eff.empty)
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
          (res, ctx, env, sto, stw) =>
              val (rs1, dw1, ef1) = x(res, ctx, env, sto, stw)
              val (rs2, dw2, ef2) = y(res, ctx, env, sto, stw)
              (rs1 ++ rs2, stw.join(dw1, dw2), ef1 ++ ef2)
        // MONADERROR
        def fail[X](err: Error) =
          mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
          (_, _, _, sto, stw) => (Set((eqA(sto), Delta.empty)), Delta.empty, Eff.empty)
        def extendSto(adr: Adr, vlu: Val) =
          (_, _, env, sto, stw) =>
              val (std, sdw) = extendV(sto, stw, adr, vlu)
              (Set(((), std)), sdw, Eff.empty)
        def updateSto(adr: Adr, vlu: Val) =
          (_, _, env, sto, stw) =>
              val (std, sdw) = updateV(sto, stw, adr, vlu)
              (Set(((), std)), sdw, Eff.empty)
        def lookupSto(adr: Adr) =
          (_, _, _, sto, stw) => (Set((lookupV(sto, stw, adr), Delta.empty)), Delta.empty, Eff.empty)
        // CTX STUFF
        def getCtx =
          (_, ctx, _, sto, stw) => (Set((ctx, Delta.empty)), Delta.empty, Eff.empty)
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
          (res, ctx, env, sto, stw) => blk(res, f(ctx), env, sto, stw)
        // ENV STUFF
        def getEnv =
          (_, _, env, sto, stw) => (Set((env, Delta.empty)), Delta.empty, Eff.empty)
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
          (res, ctx, env, sto, stw) => blk(res, ctx, f(env), sto, stw)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
          (res, ctx, env, sto, stw) =>
              val cmp = CallComponent(lam, env, ctx, sto)
              val eff = Eff.call(cmp) ++ Eff.write(stw.content.map({ case (adr, (vlu, cnt)) => (cmp.exp, ctx, adr, cnt, vlu) }))
              val (rss, dlw) = res.getOrElse(cmp, (Set.empty, Delta.empty)).asInstanceOf[(Set[(Val, Dlt)], Dlt)]
              (rss, dlw, eff)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) { intra =>
      // local state
      var results = inter.results
      var stores = inter.stores

      def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val stw = getStore(intra.stores, cmp.exp, cmp.ctx)
          val (res, stw1, eff) = eval(cmp.exp)(results, cmp.ctx, cmp.env, cmp.sto, stw)
          val rgc = collectDelta(cmp.sto, stw, res, stw1)
          // update result of the analysed component
          val old = results.get(cmp)
          if rgc != old then
              results = results + (cmp -> rgc)
              trigger(ResultDependency(cmp))
          // register dependencies on all values of the store
          stw.content.keys.foreach(adr => register(WidenedAddrDependency(cmp.exp, cmp.ctx, adr)))
          // process new components
          eff.c.foreach { cmp =>
              spawn(cmp)
              register(ResultDependency(cmp))
          }
          // process writes to widened stores
          eff.w.foreach { case (exp, ctx, adr, cnt, vlu) =>
            val prv = getStore(intra.stores, exp, ctx)
            prv.joinAt(adr, vlu, cnt) match
                case None => ()
                case Some(sto) =>
                  intra.stores = setStore(intra.stores, exp, ctx, sto)
                  trigger(WidenedAddrDependency(exp, ctx, adr))
          }

      override def doWrite(dep: Dependency): Boolean = dep match
          case ResultDependency(cmp) =>
            val old = inter.results.getOrElse(cmp, (Set.empty, Delta.empty))
            val cur = intra.results.getOrElse(cmp, (Set.empty, Delta.empty))
            if old != cur then
                inter.results += cmp -> cur
                true
            else false
          case WidenedAddrDependency(exp, ctx, adr) =>
            val oldS = inter.stores.getOrElse((exp, ctx), LocalStore.empty)
            val newS = intra.stores((exp, ctx)) // we are certain to have a store here!
            val (newV, newC) = newS.content(adr)
            oldS.joinAt(adr, newV, newC) match
                case None => false
                case Some(sto) =>
                  inter.stores = setStore(inter.stores, exp, ctx, sto)
                  true
          case _ => super.doWrite(dep)
    }

//TODO: widen resulting values
//TODO: GC at every step? (ARC?)
