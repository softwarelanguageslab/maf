package maf.util.graph

import scala.annotation.tailrec

object Tarjan:

    /**
     * Implementation of Tarjan's algorithm for finding SCCs in a graph. Implementation loosely based on https://youtu.be/TyWtx7q2D7Y.<br> Example
     * use: scc(Set(1, 2, 3, 4), Map((2, Set(3)), (3, Set(2, 4)))) returns Set(Set(2, 3)).
     *
     * @param nodes
     *   The set of edges in the graph.
     * @param edges
     *   A map, mapping each node `from` to a set of nodes `to` for which the graph contains an edge `from -> to`.
     * @tparam Node
     *   The type of a node.
     * @return
     *   A set of SCCs, where each SCC is a set of the nodes that form the SCC.
     */
    def scc[Node](nodes: Set[Node], edges: Map[Node, Set[Node]]): Set[Set[Node]] =
        var unvisited: Set[Node] = nodes
        var id = 0
        var ids: Map[Node, Int] = Map()
        var low: Map[Node, Int] = Map()
        var stk: List[Node] = List()
        while unvisited.nonEmpty do
            val node = unvisited.head
            unvisited = unvisited - node
            dft(node)

        @tailrec
        def pop(node: Node, stack: List[Node]): Unit = stack match
            case Nil => throw new Error("Unexpected empty stack")
            case `node` :: t =>
              stk = t
            case h :: t =>
              low += (h -> ids(node))
              pop(node, t)

        def dft(node: Node): Unit =
            unvisited -= node
            stk = node :: stk
            ids += (node -> id)
            low += (node -> id)
            id += 1
            edges
              .get(node)
              .foreach(_.foreach { to =>
                  if unvisited.contains(to) then dft(to)
                  if stk.contains(to) then low += (node -> math.min(low(node), low(to)))
              })
            if ids(node) == low(node) then pop(node, stk)

        // Apply the filter, to avoid singleton SCCs being included into the result if there is no self-edge.
        low.groupBy(_._2).values.map(_.keySet).filter(s => s.size > 1 || (edges.contains(s.head) && edges(s.head).contains(s.head))).toSet
