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

abstract class SchemeModFLocalFS(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics:
    inter: SchemeDomain with SchemeModFLocalSensitivity =>

    // more shorthands
    type Cmp = Component
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount
    type Res = Map[Cmp, (Val, Dlt)]
    type Sts = Map[Cmp, Sto]

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
    // RESULTS
    //

    var results: Res = Map.empty
    case class ResultDependency(cmp: Cmp) extends Dependency

    //
    // STORE STUFF
    //

    def extendV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.extend(adr, vlu)
    def updateV(sto: Sto, adr: Adr, vlu: Val): Dlt = sto.update(adr, vlu)
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

    case object StoreGC extends AbstractGarbageCollector[Sto, Adr]:
        def fresh(cur: Sto) = LocalStore.empty
        def move(addr: Adr, from: Sto, to: Sto): (Sto, Set[Adr]) =
          from.content.get(addr) match
              case None             => (to, Set.empty)
              case Some(s @ (v, _)) => (LocalStore(to.content + (addr -> s)), lattice.refs(v))

    case class DeltaGC(sto: Sto) extends AbstractGarbageCollector[Dlt, Adr]:
        def fresh(cur: Dlt) = cur.copy(delta = SmartMap.empty) //TODO: this always carries over the set of updated addrs
        def move(addr: Adr, from: Dlt, to: Dlt): (Dlt, Set[Adr]) =
          from.delta.get(addr) match
              case None =>
                sto.content.get(addr) match
                    case None         => (to, Set.empty)
                    case Some((v, _)) => (to, lattice.refs(v))
              case Some(s @ (v, _)) =>
                (to.copy(delta = to.delta + (addr -> s)), lattice.refs(v))

    def withRestrictedStore(rs: Set[Adr])(blk: A[Val]): A[Val] =
      (res, ctx, env, sto) =>
          val gcs = StoreGC.collect(sto, rs)
          val (vlu, dlt, eff) = blk(res, ctx, env, gcs)
          val dgc = DeltaGC(gcs).collect(dlt, vlu.map(lattice.refs).getOrElse(Set.empty) ++ dlt.updates)
          (vlu, sto.replay(gcs, dgc), eff)

    override protected def applyClosure(app: App, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
      withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
        super.applyClosure(app, lam, ags, fvs)
      }

    //
    // STORE WIDENING
    //

    var stores: Sts = Map.empty
    def getStore(sts: Sts, cmp: Cmp): Sto = sts.getOrElse(cmp, LocalStore.empty)
    def setStore(sts: Sts, cmp: Cmp, sto: Sto): Sts = sts + (cmp -> sto)

    override def init() =
        super.init()
        stores = setStore(stores, MainComponent, initialSto)

    case class WidenedAddrDependency(cmp: Cmp, adr: Adr) extends Dependency

    //
    // EFFECTS
    //

    case class Eff(c: Set[Cmp], w: Set[(Cmp, Adr, Cnt, Val)]):
        def ++(other: Eff): Eff = Eff(c ++ other.c, w ++ other.w)
    object Eff:
        def empty = Eff(Set.empty, Set.empty)
        def call(cmp: Cmp) = Eff(Set(cmp), Set.empty)
        def write(cmp: Cmp, adr: Adr, cnt: Cnt, vlu: Val) = Eff(Set.empty, Set((cmp, adr, cnt, vlu)))
        def write(wrs: Iterable[(Cmp, Adr, Cnt, Val)]) = Eff(Set.empty, wrs.toSet)

    //
    // ANALYSISM MONAD
    //

    type A[X] = (Res, Ctx, Env, Sto) => (Option[X], Dlt, Eff)

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
          (_, _, _, sto) => (Some(x), Delta.empty, Eff.empty)
        def map[X, Y](m: A[X])(f: X => Y) =
          (res, ctx, env, sto) =>
              val (x, dlt, eff) = m(res, ctx, env, sto)
              (x.map(f), dlt, eff)
        def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
          (res, ctx, env, sto0) =>
              val (r, dlt1, eff1) = m(res, ctx, env, sto0)
              r.map(x =>
                  val sto1 = sto0.integrate(dlt1)
                  val (y, dlt2, eff2) = f(x)(res, ctx, env, sto1)
                  (y, sto0.compose(dlt2, dlt1), eff1 ++ eff2)
              ).getOrElse((None, Delta.empty, eff1))
        // MONADJOIN
        def mbottom[X] =
          (_, _, _, _) => (None, Delta.empty, Eff.empty)
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
          (res, ctx, env, sto) =>
              val (xres, dlt1, eff1) = x(res, ctx, env, sto)
              val (yres, dlt2, eff2) = y(res, ctx, env, sto)
              val joined = (xres, yres) match
                  case (None, None)       => None
                  case (Some(x), None)    => xres
                  case (None, Some(y))    => yres
                  case (Some(x), Some(y)) => Some(Lattice[X].join(x, y))
              (joined, sto.join(dlt1, dlt2), eff1 ++ eff2)
        // MONADERROR
        def fail[X](err: Error) =
          mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
          (_, _, _, sto) => (Some(eqA(sto)), Delta.empty, Eff.empty)
        def extendSto(adr: Adr, vlu: Val) =
          (_, _, env, sto) => (Some(()), extendV(sto, adr, vlu), Eff.empty)
        def updateSto(adr: Adr, vlu: Val) =
          (_, _, env, sto) => (Some(()), updateV(sto, adr, vlu), Eff.empty)
        def lookupSto(adr: Adr) =
          (_, _, _, sto) => (sto.lookup(adr), Delta.empty, Eff.empty)
        // CTX STUFF
        def getCtx =
          (_, ctx, _, sto) => (Some(ctx), Delta.empty, Eff.empty)
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
          (res, ctx, env, sto) => blk(res, f(ctx), env, sto)
        // ENV STUFF
        def getEnv =
          (_, _, env, sto) => (Some(env), Delta.empty, Eff.empty)
        def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
          (res, ctx, env, sto) => blk(res, ctx, f(env), sto)
        // CALL STUFF
        def call(lam: Lam): A[Val] =
          (res, ctx, env, sto) =>
              val cmp = CallComponent(lam, env, ctx)
              val eff = Eff.call(cmp) ++ Eff.write(sto.content.map({ case (adr, (vlu, cnt)) => (cmp, adr, cnt, vlu) }))
              val rss = res.get(cmp)
              (rss.map(_._1), rss.map(_._2).getOrElse(Delta.empty), eff)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) { intra =>
      // local state
      var results = inter.results
      var stores = inter.stores

      def analyzeWithTimeout(timeout: Timeout.T): Unit =
          val sto = getStore(intra.stores, cmp)
          val (res, dlt, eff) = eval(cmp.exp)(results, cmp.ctx, cmp.env, sto)
          res.foreach { vlu =>
              val rgc = DeltaGC(sto).collect(dlt, lattice.refs(vlu) ++ dlt.updates)
              val old = results.getOrElse(cmp, (lattice.bottom, Delta.empty))
              if ((vlu, rgc) != old) then
                  results = results + (cmp -> (vlu, rgc))
                  trigger(ResultDependency(cmp))
          }
          // register dependencies on all values of the store
          sto.content.keys.foreach(adr => register(WidenedAddrDependency(cmp, adr)))
          // process new components
          eff.c.foreach { cmp =>
              spawn(cmp)
              register(ResultDependency(cmp))
          }
          // process writes to widened stores
          eff.w.foreach { case (cmp, adr, cnt, vlu) =>
            val prv = getStore(intra.stores, cmp)
            prv.joinAt(adr, vlu, cnt) match
                case None => ()
                case Some(sto) =>
                  intra.stores = setStore(intra.stores, cmp, sto)
                  trigger(WidenedAddrDependency(cmp, adr))
          }

      override def doWrite(dep: Dependency): Boolean = dep match
          case ResultDependency(cmp) =>
            val old = inter.results.getOrElse(cmp, (lattice.bottom, Delta.empty))
            val cur = intra.results(cmp) // we are certain to have a result here!
            if old != cur then
                inter.results += cmp -> cur
                true
            else false
          case WidenedAddrDependency(cmp, adr) =>
            val oldS = inter.stores.getOrElse(cmp, LocalStore.empty)
            val newS = intra.stores(cmp) // we are certain to have a store here!
            val (newV, newC) = newS.content(adr)
            oldS.joinAt(adr, newV, newC) match
                case None => false
                case Some(sto) =>
                  inter.stores = setStore(inter.stores, cmp, sto)
                  true
          case _ => super.doWrite(dep)
    }

//TODO: widen resulting values
//TODO: GC at every step? (ARC?)

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
