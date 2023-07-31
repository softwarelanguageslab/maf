package maf.modular.scheme.monadic

import maf.language.scheme.*
import maf.modular.AnalysisEntry
import maf.core.BasicStore
import maf.core.worklist.FIFOWorkList
import maf.core.Monad.*
import maf.modular.AddrDependency
import maf.modular.ReturnAddr
import maf.core.{Address, Environment}
import maf.modular.scheme.PrmAddr
import maf.modular.scheme.modflocal.SchemeModFLocalNoSensitivity
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.util.benchmarks.Timeout.T
import maf.core.IdentityMonad
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.modular.scheme.SchemeDomain
import maf.core.Identity
import maf.core.Monad
import maf.core.IdentityMonad.Id
import maf.util.TrampolineT
import maf.util.TrampolineT.TrampolineM
import maf.util.Trampoline

abstract class ModF[M[_]: Monad](exp: SchemeExp) extends SchemeModFLocalSensitivity, SchemeDomain, Monolith:
    type A[X] = SuspendM[X]

    final lazy val initialBds: Iterable[(String, Address, Value)] = primitives.allPrimitives.map { case (name, p) =>
        (name, PrmAddr(name), lattice.primitive(p.name))
    }

    private lazy val initialEnv: Env = Environment(initialBds.map(bnd => (bnd._1, bnd._2)))
    private lazy val initialSto: Sto =
        BasicStore.empty.extend(initialBds.map { case (_, a, v) => (a -> v) })

    private def body(e: Component): SchemeExp = e match
        case Main              => exp
        case Call(lam, _, ctx) => SchemeBegin(lam.body, Identity.none)

    private def expr(e: Component): SchemeExp = e match
        case Main              => exp
        case Call(lam, _, ctx) => lam

    private def env(e: Component): Env = e match
        case Main              => initialEnv
        case Call(lam, env, _) => env

    private def ctx(e: Component): Ctx = e match
        case Main            => initialCtx
        case Call(_, _, ctx) => ctx

    private def prepareNext(result: Effects): Effects =
        val cmps =
            // spawn all components in C
            result.C.filterNot(c => result.seen.contains(c)) ++
                // add all components that are triggered
                result.R.filter { case (r, _) => result.W.contains(r) }.flatMap { case (_, k) => k }

        // add all spawns to the seen state set
        val seen = result.seen ++ result.C

        // reset C and W, update seen and add components to worklist
        result.copy(C = Set(), W = Set(), seen = seen, wl = result.wl.addAll(cmps))

    // a suspendable monad instance
    final protected val suspendable: Suspend = new Suspend { type State = Effects }

    override def eval(exp: SchemeExp): SuspendM[Val] = exp match
        case _ =>  
          suspend(exp) >>> super.eval(exp)

    given MonadFix_[suspendable.Suspend, Effects] with
        type M[X] = suspendable.Suspend[X]
        import suspendable.suspendMonad.*
        def init: M[Effects] = unit(Effects(cmp = Main, sto = initialSto, wl = FIFOWorkList.empty.add(Main)))
        override def hasChanged(prev: Effects, next: Effects): M[Boolean] =
            // the algorithm completes when the worklist is empty
            unit(next.wl.nonEmpty)
        def step(e: Effects): M[Effects] =
            println(e.wl)
            if e.wl.isEmpty then unit(e)
            else
                val next = e.wl.head
                val result =
                    SuspendM.run(eval(body(next)))(env(next), ctx(next), e.copy(cmp = next, wl = e.wl.tail, C = Set(), W = Set())) match
                        case (e2, None) =>
                            e2
                        case (e2, Some(v)) =>
                            val ret = ReturnAddr(next, expr(next).idn)
                            if lattice.subsumes(e2.sto.lookup(ret).getOrElse(lattice.bottom), v) then e2
                            else e2.copy(W = e.W + AddrDependency(ret), sto = e2.sto.extend(ret, v))

                unit(prepareNext(result))

class SimpleModFAnalysis(prg: SchemeExp)
    extends ModF[IdentityMonad.Id](prg),
      SchemeModFLocalNoSensitivity,
      SchemeConstantPropagationDomain,
      AnalysisEntry[SchemeExp] {

    private var _result: Option[Any] = None
    private var _finished: Boolean = false
    override def result: Option[Any] = _result
    override protected def analysisM: AnalysisM[A] = suspendAnalysisM
    override def finished: Boolean = _finished
    override def printResult: Unit = if finished then println(result.get)
    override def analyzeWithTimeout(timeout: T): Unit =
        val effects = MonadFix.fix[suspendable.Suspend, Effects, Any].run
        _finished = effects.wl.isEmpty
        _result = Some(effects.sto.lookup(ReturnAddr(Main, prg.idn)).getOrElse(lattice.bottom))
}

