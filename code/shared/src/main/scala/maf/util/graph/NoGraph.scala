package maf.util.graph

/** A graph that preserves no information */
class NoGraph[N <: GraphElement, E <: GraphElement, S <: GraphElement]:
    case class G()
    object G:
        implicit val typeclass: Graph[G, N, E, S] = new Graph[G, N, E, S] {
            def empty = G()
            def addNode(g: G, node: N) = g
            def addEdge(
                g: G,
                node1: N,
                edge: E,
                node2: N
              ) = g
            override def addEdges(g: G, l: Iterable[(N, E, N)]) = g
            override def addSubgraph(g: G, spec: S, nodes: Set[N]): G = ??? // TODO
            def removeNode(g: G, node: N) = g
            def removeEdge(
                g: G,
                node1: N,
                edge: E,
                node2: N
              ) = g
            def nodes(g: G) = 0
            def edges(g: G) = 0
            def findNodes(g: G, p: N => Boolean) = Set.empty
            def setName(g: G, name: String): G = g
        }
