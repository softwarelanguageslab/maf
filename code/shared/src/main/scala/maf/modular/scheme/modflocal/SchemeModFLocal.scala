package maf.modular.scheme.modflocal

import maf.modular.ModAnalysis
import maf.modular.scheme._
import maf.core._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.util.benchmarks.Timeout
import maf.language.CScheme._
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.LatticeWithAddrs

abstract class SchemeModFLocal(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeSemantics {
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  // more shorthands
  type Cmp = Component
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
  def extendV(sto: Sto, adr: Adr, vlu: Val): Sto = sto.extend(adr, V(vlu))
  def updateV(sto: Sto, adr: Adr, vlu: Val): Sto = sto.update(adr, V(vlu))

  def lookupE(sto: Sto, adr: Adr): Set[Env] = sto(adr).asInstanceOf[E].evs
  def extendE(sto: Sto, adr: Adr, evs: Set[Env]): Sto = sto.extend(adr, E(evs))

  //
  // COMPONENTS
  //

  sealed trait Component extends Serializable {
    def exp: Exp
    def env: Env
    def sto: Sto
    def ctx: Ctx
  }
  case object MainComponent extends Component {
    def exp = initialExp
    def env = initialEnv
    def sto = initialSto
    def ctx = initialCtx
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

  var results: Map[Component, Set[(Val, Sto)]] = Map.empty
  def addResult(cmp: Component, res: Set[(Val, Sto)]) =
    results += cmp -> (getResult(cmp) ++ res)
  def getResult(cmp: Component): Set[(Val, Sto)] =
    results.getOrElse(cmp, Set.empty)

  //
  // ANALYSISM MONAD
  //
  
  case class A[X](run: (Ctx, Env, Sto) => Set[(X, Sto)])

  implicit val anl: AnalysisM[A] = new AnalysisM[A] {
    // MONAD
    def unit[X](x: X) = 
      A((_, _, sto) => Set((x, sto)))
    def map[X, Y](m: A[X])(f: X => Y) = 
      A((ctx, env, sto) => m.run(ctx, env, sto).map(res => (f(res._1), res._2)))
    def flatMap[X, Y](m: A[X])(f: X => A[Y]) = 
      A((ctx, env, sto) => m.run(ctx, env, sto).flatMap(res => f(res._1).run(ctx, env, res._2)))
    // MONADJOIN
    def mbottom[X] = 
      A((_,_,_) => Set.empty)
    def mjoin[X: Lattice](x: A[X], y: A[X]) = 
      A((ctx, env, sto) => x.run(ctx, env, sto) ++ y.run(ctx, env, sto))
    // MONADERROR
    def fail[X](err: Error) = 
      mbottom // we are not interested in errors here (at least, not yet ...)
    // STOREM
    def addrEq = 
      A((_, _, sto) => Set((sto.addrEq, sto)))
    def extendSto(adr: Adr, vlu: Val) = 
      A((_, env, sto) => Set(((), extendV(sto, adr, vlu))))
  	def updateSto(adr: Adr, vlu: Val) = 
      A((_, env, sto) => Set(((), updateV(sto, adr, vlu))))
    def lookupSto(adr: Adr) = 
      A((_, _, sto) => Set((lookupV(sto, adr), sto)))
    // ANALYSISM
    def getCtx = 
      A((ctx, _, sto) => Set((ctx, sto)))
    // ENV STUFF
    def getEnv = 
      A((_, env, sto) => Set((env, sto)))
    def withExtendedEnv[X](nam: String, adr: Adr)(blk: A[X]): A[X] = 
      A((ctx, env, sto) => blk.run(ctx, env.extend(nam, adr), sto))
	  def extendEnvSto(adr: EnvAddr, evs: Set[Env]) = 
      A((_, _, sto) => Set(((), extendE(sto, adr, evs))))
    def lookupEnvSto(adr: EnvAddr) =
      A((_, _, sto) => Set((lookupE(sto, adr), sto)))
    def call(lam: Lam, ctx: Ctx): A[Val] = 
      A((_, _, sto) => getResult(CallComponent(lam, ctx, sto)))
  } 

  def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) {
    def analyzeWithTimeout(timeout: Timeout.T): Unit =
      addResult(cmp, eval(cmp.exp).run(cmp.ctx, cmp.env, cmp.sto))
  }
}
