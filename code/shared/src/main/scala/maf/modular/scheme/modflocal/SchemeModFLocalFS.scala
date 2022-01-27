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
    type Cll = CallComponent
    type Dep = Dependency
    type Sto = LocalStore[Adr, Val]
    type Dlt = Delta[Adr, Val]
    type Cnt = AbstractCount
    type Res = Map[Cmp, (Val, Dlt)]
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
      (anl, env, sto, ctx) =>
          val gcs = StoreGC.collect(sto, rs)
          blk(anl, env, sto, ctx).map { (vlu, dlt) =>
              val gcd = DeltaGC(gcs).collect(dlt, lattice.refs(vlu) ++ dlt.updates)
              (vlu, gcd)
          }

    override protected def applyClosure(app: App, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
      withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ fvs.flatMap((_, vlu) => lattice.refs(vlu))) {
        super.applyClosure(app, lam, ags, fvs)
      }

    override def init() =
        super.init()
        stores += MainComponent -> initialSto

    //
    // ANALYSISM MONAD
    //

    type A[X] = (Anl, Env, Sto, Ctx) => Option[(X, Dlt)]

    given analysisM: AnalysisM[A] with
        // MONAD
        def unit[X](x: X) =
          (_, _, _, _) => Some((x, Delta.empty))
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
          (_, _, _, _) => None
        def mjoin[X: Lattice](x: A[X], y: A[X]) =
          (anl, env, sto, ctx) =>
            (x(anl, env, sto, ctx), y(anl, env, sto, ctx)) match
                case (None, yres)                   => yres
                case (xres, None)                   => xres
                case (Some((xv, xs)), Some(yv, ys)) => (Some((Lattice[X].join(xv, yv), sto.join(xs, ys))))
        // MONADERROR
        def fail[X](err: Error) =
          mbottom // we are not interested in errors here (at least, not yet ...)
        // STOREM
        def addrEq =
          (_, _, sto, _) => Some((eqA(sto), Delta.empty))
        def extendSto(adr: Adr, vlu: Val) =
          (_, _, sto, _) => Some((), extendV(sto, adr, vlu))
        def updateSto(adr: Adr, vlu: Val) =
          (_, _, sto, _) => Some((), updateV(sto, adr, vlu))
        def lookupSto(adr: Adr) =
          (_, _, sto, _) => sto.lookup(adr).map((_, Delta.empty))
        // CTX STUFF
        def getCtx =
          (_, _, _, ctx) => Some((ctx, Delta.empty))
        def withCtx[X](f: Ctx => Ctx)(blk: A[X]) =
          (anl, env, sto, ctx) => blk(anl, env, sto, f(ctx))
        // ENV STUFF
        def getEnv =
          (_, env, _, _) => Some((env, Delta.empty))
        def withEnv[X](f: Env => Env)(blk: A[X]) =
          (anl, env, sto, ctx) => blk(anl, f(env), sto, ctx)
        // CALL STUFF
        def call(lam: Lam) =
          (anl, env, sto, ctx) => anl.call(CallComponent(lam, env, ctx), sto)

    //
    // THE INTRA-ANALYSIS
    //

    def intraAnalysis(cmp: Component) = new SchemeModFLocalFSIntraAnalysis(cmp)
    class SchemeModFLocalFSIntraAnalysis(cmp: Component) extends IntraAnalysis(cmp):
        intra =>

        // local state
        var results = inter.results
        var stores = inter.stores

        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            // get the (widened) store
            val sto = stores.getOrElse(cmp, LocalStore.empty)
            // register dependencies on all addresses
            sto.content.keys.foreach(adr => register(AddrDependencyFS(cmp, adr)))
            // GC the result
            val rgc = eval(cmp.exp)(this, cmp.env, sto, cmp.ctx).map { (v, d) =>
              (v, DeltaGC(sto).collect(d, lattice.refs(v) ++ d.updates))
            }
            // update the result of this component
            val old = results.get(cmp)
            if (rgc != old) then
                results += cmp -> rgc.get
                trigger(ResultDependency(cmp))

        def call(cll: Cll, sto: Sto): Option[(Val, Dlt)] =
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
            // read its result
            register(ResultDependency(cll))
            results.get(cll)

        override def doWrite(dep: Dependency): Boolean = dep match
            case ResultDependency(cmp) =>
              val old = inter.results.getOrElse(cmp, (lattice.bottom, Delta.empty))
              val cur = intra.results(cmp) // we are certain to have a result here!
              if old != cur then
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
