package maf.aam.scheme.optimisations

import maf.aam.scheme.*

import maf.language.scheme.*
import maf.core.Monad.*
import maf.util.Trampoline.{done, tailcall}

/**
 * Trait that internally steps if the given state does not cross function boundaries. This greatly reduces the number of elements in the set of seen
 * states of the analysis, which should help with performance.
 */
trait SchemeFunctionCallBoundary extends BaseSchemeAAMSemantics:
    override def stepDirect(ss: Set[State]): Result =
        import Control.*
        ss.map {
          // for widening we explitily to not step internally, so that the widening is propagated to the other configurations
          case s @ SchemeState(Ret(WidenAddr(_, _)), _, _, _, _) =>
            done(Set(s))
          case s @ SchemeState(Ap(_) | Ret(_), _, kon, _, _) if readKonts(s.s, kon).map(_._1).collectFirst { case HltFrame => }.isDefined =>
            done(Set(s)) // normal halting state
          case s @ SchemeState(HltE(_), _, _, _, _) => done(Set(s)) // halting error state

          case s @ SchemeState(Hlt(_), _, _, _, _) => done(Set(s)) // normal halting state
          case s @ SchemeState(Ap(_) | Ret(_), _, _: FunRetAddr, _, _) =>
            done(Set(s)) // return from function
          case s @ SchemeState(Fn(_), _, _, _, _) => done(Set(s)) // enter function
          case s =>
            println(s"trying to step $s")
            step(s) // al others can step internally without generating states
        }.foldSequence(Set())((successors, all) => done(all ++ successors))

    override def ap(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Result =
      // unless we continue with a function return address or are at a halting frame, we can simply
      // pas this information to the `continue` function, avoiding allocating a seperate state in memory.
      kont match
          case _: FunRetAddr => super.ap(value, sto, kont, t, ext) // function return
          case _ if readKonts(sto, kont).map(_._1).collectFirst { case HltFrame => }.isDefined => super.ap(value, sto, kont, t, ext) // halting frame
          case _ => tailcall(continue(value, sto, kont, t, ext)) // continue directly if we can without allocating a new state

    override def ev(
        exp: SchemeExp,
        env: Env,
        sto: Sto,
        kont: KonA,
        timestamp: Timestamp,
        ext: Ext,
        call: Boolean = false
      ): Result =
      // unless we enter a function, we can directly jump to the evaluation function without generating a new state
      if call then done(Set(SchemeState(Control.Fn(Control.Ev(exp, env)), sto, kont, timestamp, ext)))
      else tailcall(eval(exp, env, sto, kont, timestamp, ext))

    override def pushFrame(
        e: Expr,
        env: Env,
        sto: Sto,
        next: KonA,
        frame: Kont,
        t: Timestamp
      ): (Sto, KonA, Timestamp) =
      // pushFrame is only supposed to be called within function boundaries, hence, the continuations within function boundaries do not need to be store allocated
      (sto, frame.link(next), t)
