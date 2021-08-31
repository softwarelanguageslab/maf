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

abstract class SchemeModFLocal(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics {
  inter: SchemeDomain with SchemeModFLocalSensitivity =>

  // more shorthands
  type Cmp = Component
  type Dep = Dependency
  type Sto = LocalStore[Adr, Storable] // TODO: split the store?

  //
  // INITIALISATION
  //

  lazy val initialExp: Exp = program
  lazy val initialEnv: Env = NestedEnv(initialBds.map(p => (p._1, p._2)).toMap, None)
  lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))

  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    CSchemeUndefiner.undefine(List(preludedProgram))
  }

  private lazy val initialBds: Iterable[(String, Adr, Storable)] =
    primitives.allPrimitives.view
      .filterKeys(initialExp.fv)
      .map { case (name, p) =>
        (name, PrmAddr(name), V(lattice.primitive(p.name)))
      }

  //
  // THE STORE
  //

  // the store is used for several purposes:
  // - mapping variable/pointer addresses to values
  // - mapping environment addresses to environments
  sealed trait Storable
  case class V(vlu: Value) extends Storable
  case class E(evs: Set[Env]) extends Storable

  implicit def storableLattice: LatticeWithAddrs[Storable, Adr] = new LatticeWithAddrs[Storable, Adr] {
    def bottom: Storable = throw new Exception("No single bottom element in Storable lattice")
    override def isBottom(x: Storable) = x match {
      case V(vlu) => vlu == lattice.bottom
      case E(evs) => evs.isEmpty
    }
    def join(x: Storable, y: => Storable): Storable = (x, y) match {
      case (V(v1), V(v2)) => V(lattice.join(v1, v2))
      case (E(e1), E(e2)) => E(e1 ++ e2)
      case _              => throw new Exception(s"Attempting to join incompatible elements $x and $y")
    }
    def refs(x: Storable): Set[Adr] = x match {
      case V(vlu) => lattice.refs(vlu)
      case E(evs) => evs.flatMap(_.addrs)
    }
    def top: Storable = throw new Exception("No top element in Storable lattice")
    def subsumes(x: Storable, y: => Storable): Boolean = throw new Exception("NYI")
    def eql[B: BoolLattice](x: Storable, y: Storable): B = throw new Exception("NYI")
    def show(v: Storable): String = v.toString
  }

  def lookupV(sto: Sto, adr: Adr): Val = sto(adr).asInstanceOf[V].vlu
  def extendV(sto: Sto, adr: Adr, vlu: Val): sto.DeltaStore = sto.deltaStore.extend(adr, V(vlu))
  def updateV(sto: Sto, adr: Adr, vlu: Val): sto.DeltaStore = sto.deltaStore.update(adr, V(vlu))

  def lookupE(sto: Sto, adr: Adr): Set[Env] = sto(adr).asInstanceOf[E].evs
  def extendE(sto: Sto, adr: Adr, evs: Set[Env]): sto.DeltaStore = sto.deltaStore.extend(adr, E(evs))

  //
  // COMPONENTS
  //

  sealed trait Component extends Serializable {
    def exp: Exp
    def env: Env
    val sto: Sto
    val ctx: Ctx
  }
  case object MainComponent extends Component {
    val exp = initialExp
    val env = initialEnv
    val sto = initialSto
    val ctx = initialCtx
    override def toString = "main"
  }
  case class CallComponent(lam: Lam, ctx: Ctx, sto: Sto) extends Component {
    def exp = SchemeBody(lam.body)
    def env = {
      val fixedArgEnv = lam.args.map(par => (par.name, VarAddr(par, ctx))).toMap
      val varArgEnv = lam.varArgId.map(par => (par.name, (VarAddr(par, ctx)))).toMap
      NestedEnv(fixedArgEnv ++ varArgEnv, Some(EnvAddr(lam, ctx)))
    }
    override def toString = s"${lam.lambdaName} [$ctx] [$sto]"
  }

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
    A { (res, ctx, env, sto) =>
      val (rss, cps, rds, wds) = blk.run(res, ctx, env, sto.collect(rs))
      (rss.map { case (x, d) => (x, sto.replay(d)) }, cps, rds, wds)
    }

  override protected def applyClosure(cll: Cll, clo: Clo, ags: List[Val]): A[Val] =
    withRestrictedStore(ags.flatMap(lattice.refs).toSet ++ clo._2.addrs) {
      super.applyClosure(cll, clo, ags)
    }

  //
  // ANALYSISM MONAD
  //

  // TODO: more type safety coming with path-dependent functions in Scala 3!
  case class A[X](run: (res: Res, ctx: Ctx, env: Env, sto: Sto) => (Set[(X, sto.DeltaStore)], Set[Cmp], Set[Dep], Set[Dep]))

  implicit override val analysisM: AnalysisM[A] = new AnalysisM[A] {
    // MONAD
    def unit[X](x: X) =
      A((_, _, _, sto) => (Set((x, sto.deltaStore)), Set(), Set(), Set()))
    def map[X, Y](m: A[X])(f: X => Y) =
      A { (res, ctx, env, sto) =>
        val (rss, cps, rds, wds) = m.run(res, ctx, env, sto)
        (rss.map(res => (f(res._1), res._2)), cps, rds, wds)
      }
    def flatMap[X, Y](m: A[X])(f: X => A[Y]) =
      A { (res, ctx, env, sto) =>
        val (rs1, cs1, rd1, wd1) = m.run(res, ctx, env, sto)
        rs1.foldLeft((Set[(Y, sto.DeltaStore)](), cs1, rd1, wd1)) { case (acc, (x, d0)) =>
          val (rs2, cs2, rd2, wd2) = f(x).run(res, ctx, env, sto.integrate(d0))
          val rs3 = rs2.map { case (x, d1) => (x, sto.compose(d1, d0)) }
          (acc._1 ++ rs3, acc._2 ++ cs2, acc._3 ++ rd2, acc._4 ++ wd2)
        }
      }
    // MONADJOIN
    def mbottom[X] =
      A((_, _, _, _) => (Set(), Set(), Set(), Set()))
    def mjoin[X: Lattice](x: A[X], y: A[X]) =
      A { (res, ctx, env, sto) =>
        val (rs1, cs1, rd1, wd1) = x.run(res, ctx, env, sto)
        val (rs2, cs2, rd2, wd2) = y.run(res, ctx, env, sto)
        (rs1 ++ rs2, cs1 ++ cs2, rd1 ++ rd2, wd1 ++ wd2)
      }
    // MONADERROR
    def fail[X](err: Error) =
      mbottom // we are not interested in errors here (at least, not yet ...)
    // STOREM
    def addrEq =
      A((_, _, _, sto) => (Set((sto.addrEq, sto.deltaStore)), Set(), Set(), Set()))
    def extendSto(adr: Adr, vlu: Val) =
      A((_, _, env, sto) => (Set(((), extendV(sto, adr, vlu))), Set(), Set(), Set()))
    def updateSto(adr: Adr, vlu: Val) =
      A((_, _, env, sto) => (Set(((), updateV(sto, adr, vlu))), Set(), Set(), Set()))
    def lookupSto(adr: Adr) =
      A((_, _, _, sto) => (Set((lookupV(sto, adr), sto.deltaStore)), Set(), Set(), Set()))
    // ANALYSISM
    def getCtx =
      A((_, ctx, _, sto) => (Set((ctx, sto.deltaStore)), Set(), Set(), Set()))
    // ENV STUFF
    def getEnv =
      A((_, _, env, sto) => (Set((env, sto.deltaStore)), Set(), Set(), Set()))
    def withExtendedEnv[X](nam: String, adr: Adr)(blk: A[X]): A[X] =
      A((res, ctx, env, sto) => blk.run(res, ctx, env.extend(nam, adr), sto))
    def extendEnvSto(adr: EnvAddr, evs: Set[Env]) =
      A((_, _, _, sto) => (Set(((), extendE(sto, adr, evs))), Set(), Set(), Set()))
    def lookupEnvSto(adr: EnvAddr) =
      A((_, _, _, sto) => (Set(((lookupE(sto, adr), sto.deltaStore))), Set(), Set(), Set()))
    def call(lam: Lam, ctx: Ctx): A[Val] =
      A { (res, _, _, sto) =>
        val cmp = CallComponent(lam, ctx, sto)
        val rss = res.getOrElse(cmp, Set.empty).asInstanceOf[Set[(Val, sto.DeltaStore)]]
        (rss, Set(cmp), Set(ResultDependency(cmp)), Set())
      }
  }

  //
  // THE INTRA-ANALYSIS
  //

  def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) { intra =>
    var results = inter.results

    def analyzeWithTimeout(timeout: Timeout.T): Unit = {
      val (res, cps, rds, wds) = eval(cmp.exp).run(results, cmp.ctx, cmp.env, cmp.sto)
      val old = results.get(cmp)
      if (res != old) {
        results = results + (cmp -> res)
        trigger(ResultDependency(cmp))
      }
      cps.foreach(spawn)
      rds.foreach(register)
      assert(wds.isEmpty)
    }
    override def doWrite(dep: Dependency): Boolean = dep match {
      case ResultDependency(cmp) =>
        val old = inter.results.getOrElse(cmp, Set.empty)
        val cur = intra.results.getOrElse(cmp, Set.empty)
        if (old != cur) {
          inter.results += cmp -> results.getOrElse(cmp, Set.empty)
          true
        } else {
          false
        }
      case _ => super.doWrite(dep)
    }
  }
}
