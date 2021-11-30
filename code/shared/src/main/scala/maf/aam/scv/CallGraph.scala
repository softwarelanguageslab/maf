package maf.aam.scv

import maf.core.{Address, Environment}
import maf.language.scheme.*

object CallGraph:
    type CallTarget = (SchemeLambdaExp, Environment[Address])

    case class Edge(target: CallTarget)

    /**
     * An enumeration type that is either safe (e.g. the call graph contains no loops) or recursive (the call graph contains loops).
     *
     * If <code>Safe</code> the call graph is returned, if not a list of call targets is returned uptil the point where the recurring target is
     */
    enum Looped:
        case Safe(c: CallGraph)
        case Recursive(stack: List[CallTarget])

    /** Checks whether the given value is recursive or not */
    def isLooping(e: Looped): Boolean = e match
        case Looped.Safe(_)      => false
        case Looped.Recursive(_) => true

    /** Create an empty call graph */
    def empty: CallGraph = CallGraph(List())

/** A datastructure that keeps track of the call history. */
case class CallGraph(stack: List[CallGraph.CallTarget]):
    import CallGraph.*

    /**
     * Add an edge to the given call graph.
     *
     * @param edge
     *   the edge to add to the call graph
     * @return
     *   a <code>Looped</code> type, that contains the updated graph (if it is not looped) or the graph upto the looped point
     */
    def add(edge: Edge): Looped =
      if stack.contains(edge.target) then Looped.Recursive(stack.takeWhile(_ == edge.target))
      else Looped.Safe(this.copy(stack = edge.target :: stack))

    def pop: CallGraph =
      if stack.isEmpty then CallGraph(stack) else CallGraph(stack.tail)
