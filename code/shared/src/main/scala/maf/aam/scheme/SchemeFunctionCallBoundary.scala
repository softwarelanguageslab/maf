package maf.aam.scheme

/**
 * Trait that internally steps if the given state does not cross function boundaries. This greatly reduces the number of elements in the set of seen
 * states of the analysis, which should help with performance.
 */
trait SchemeFunctionCallBoundary extends SchemeAAMSemantics:
    override def stepDirect(ss: Set[State]): Set[State] =
        import Control.*
        ss.flatMap {
          case s @ SchemeState(Fn(_), _, _, _, _) => Set(s)
          case s @ SchemeState(Ap(_), _, _, _, _) => Set(s)
          // TODO: allow for more internal stepping
          case s => step(s)
        }
