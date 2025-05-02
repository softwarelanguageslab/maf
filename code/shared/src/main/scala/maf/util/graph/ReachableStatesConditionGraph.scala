package maf.util.graph

case class ReachableStatesConditionGraph[N <: GraphElement, E <: GraphElement, S <: GraphElement](
    condition: N => Boolean):
    case class G(states: Set[N])

    object G:
        implicit val typeclass: Graph[G, N, E, S] = new Graph[G, N, E, S] {
            def empty = new G(Set.empty)
            def addNode(g: G, node: N) =
                if condition(node) then G(g.states + node)
                else g
            def addEdge(
                g: G,
                node1: N,
                edge: E,
                node2: N
              ) = addNode(addNode(g, node1), node2)
            def removeNode(g: G, node: N) = G(g.states - node)
            def addSubgraph(g: G, spec: S, nodes: Set[N]): G = ??? // TODO
            def removeEdge(
                g: G,
                node1: N,
                edge: E,
                node2: N
              ) = g
            def nodes(g: G) = g.states.size
            def edges(g: G) = 0
            def findNodes(g: G, p: N => Boolean) = g.states.filter(p)
            def setName(g: G, name: String): G = setName(g, name)
        }
