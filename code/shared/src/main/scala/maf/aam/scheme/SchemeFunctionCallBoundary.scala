package maf.aam.scheme

/**
 * Trait that internally steps if the given state does not cross function boundaries. This greatly reduces the number of elements in the set of seen
 * states of the analysis, which should help with performance.
 */
trait SchemeFunctionCallBoundary extends SchemeAAMSemantics:
    override def stepDirect(ss: Set[State]): Set[State] =
        import Control.*
        ss.flatMap {
          case s @ SchemeState(Ap(_) | Ret(_), _, kon, _, _) if readKonts(s.s, kon).collectFirst { case HltFrame => }.isDefined => Set(s) // normal halting state
          case s @ SchemeState(HltE(_), _, _, _, _) => Set(s) // halting error state
          case s @ SchemeState(Ap(_) | Ret(_), _, _: FunRetAddr, _, _) =>
            Set(s) // return from function
          case s @ SchemeState(Fn(_), _, _, _, _) => Set(s) // enter function
          case s                                  => step(s) // al others can step internally without generating states
        }

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
