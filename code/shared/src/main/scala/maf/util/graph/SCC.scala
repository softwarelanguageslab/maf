package maf.util.graph

import maf.language.scheme.SchemeCase
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion

import scala.annotation.tailrec

object SCC:

    /**
     * Collapses a graph into its strongly connected components.
     *
     * @param nodes
     *   The set of nodes in the graph.
     * @param edges
     *   A map, mapping each node `from` to a set of nodes `to` for which the graph contains an edge `from -> to`.
     * @see
     *   scc for the algorithm
     */
    def collapse[N](nodes: Set[N], originalEdges: Map[N, Set[N]]): (Set[Set[N]], Map[Set[N], Set[Set[N]]]) =
        val cmpsWithoutSelf = tarjan(nodes, originalEdges)
        // create singleton sets of nodes that are not in an scc
        val cmps = cmpsWithoutSelf ++ nodes.filterNot(n => cmpsWithoutSelf.exists(_.contains(n))).map(Set(_))
        val edges = cmps.foldLeft(Map[Set[N], Set[Set[N]]]().withDefaultValue(Set()))((edges, cmp) =>
            cmp.foldLeft(edges)((edges, n) =>
                cmps.filter(toCmp => originalEdges.get(n).getOrElse(Set()).intersect(toCmp).nonEmpty)
                    .foldLeft(edges)((edges, toCmp) => if cmp != toCmp then edges + (cmp -> (edges(cmp) + toCmp)) else edges)
            )
        )

        (cmps, edges)

    /**
     * Implementation of Tarjan's algorithm for finding SCCs in a graph. Implementation loosely based on https://youtu.be/TyWtx7q2D7Y.<br>
     * Example use: scc(Set(1, 2, 3, 4), Map((2, Set(3)), (3, Set(2, 4)))) returns Set(Set(2, 3)).<br>
     * The algorithm operates in O(|V|+|E|).
     *
     * @param nodes The set of nodes in the graph.
     * @param edges A map, mapping each node `from` to a set of nodes `to` for which the graph contains an edge `from -> to`.
     * @tparam Node The type of a node.
     * @return A set of SCCs, where each SCC is a set of the nodes that form the SCC.
     * @note Unit tests: maf.test.util.TarjanSCCTests
     */
    def tarjan[Node](nodes: Set[Node], edges: Map[Node, Set[Node]]): Set[Set[Node]] =
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
        low.groupBy(_._2).map(_._2.keySet).filter(s => s.size > 1 || (edges.contains(s.head) && edges(s.head).contains(s.head))).toSet

    /**
     * Incrementally computes the updated set of SCCs in an updated graph.
     * @param nodes The set of nodes in the updated graph.
     * @param edges The set of edges in the updated graph.
     *              Edges must only contain all current edges, including the ones in addedEdges but excluding the ones in removedEdges.
     * @param addedEdges The edges added to the graph.
     * @param removedEdges The edges removed from the graph.
     * @param previousSCCs The SCCs computed for the previous version of the graph.
     * @tparam Node The type of nodes in the graph.
     * @return A set of SCCs, where each SCC is a set of the nodes that form the SCC.
     * @note Unit tests: maf.test.util.IncrementalSCCTests
     */
    def incremental[Node](nodes: Set[Node], edges: Map[Node, Set[Node]], addedEdges: Map[Node, Set[Node]], removedEdges: Map[Node, Set[Node]], previousSCCs: Set[Set[Node]]): Set[Set[Node]] =
        // findPaths returns all nodes on all paths from from to to as well as the set of visited nodes (= all nodes reachable from from).
        // Works in O(|V|+|E|), although smaller in practice because it only traverses part of the graph reachable from current.
        def findPaths(current: Node, target: Node, paths: Set[Node] = Set(), visited: Set[Node] = Set()): (Set[Node], Set[Node]) =
            if current == target || paths.contains(current)
            then (Set(current), visited + current) // Found: add the node to the path list and to the visited set.
            else if visited.contains(current)
            then (Set(), visited) // Not found, we are looping! Only add the node to the visited set.
            else // Explore where we can get from the current node.
                val next = edges.getOrElse(current, Set())
                // Traverse the entire graph only once depht-first, by accumulating all visited nodes, and
                // whether they have a path to the target or not.
                val (newPaths, vis) =  next.foldLeft((paths, visited + current)) { case ((paths, vis), next) =>
                    val (path, vis2) = findPaths(next, target, paths, vis)
                    if path.isEmpty
                    then (paths, vis2) else (SmartUnion.sunion(path, paths), vis2)
                }
                // If any of the successor nodes has a path to the target, then so does this node.
                // We cannot just check whether newPaths is non-empty, as it contains the accumulated paths.
                if newPaths.intersect(next).nonEmpty then (newPaths + current, vis) else (newPaths, vis)

        var currSCCs = previousSCCs

        // Treat edge removals: find the SCCs for which an edges between two nodes is removed. (The other removals cannot cause SCCs to be removed.)
        // For every SCC that is found, verify whether it still is an SCC. If it is split up, remove it.
        removedEdges.flatMap { case (source, targets) =>
            currSCCs.filter(scc => scc.contains(source) && scc.intersect(targets).nonEmpty)
            //targets.flatMap(target => currSCCs.find{scc => scc.contains(source) && scc.contains(target)})
        }.toSet.foreach { SCC => // toSet to avoid duplicate traversals.
            val newSCC = tarjan(SCC, edges) // Local Tarjan using the nodes of the SCC.
            if newSCC.size != 1 || newSCC.head != SCC // Not sure whether this test makes it faster.
            then currSCCs = SmartUnion.sunion(currSCCs - SCC, newSCC)
        }

        // Treat edge additions: look for edges not within an SCC that are added. (The others can't create a new SCC.)
        // To do so, look remove the targets that belong to the same SCC as the sources.
        // For all edges added not within an SCC, verify whether an SCC is formed.
        // If so, create the new SCC, but keep into account that all nodes in the path may themselves already be in an SCC.
        // TODO: Maybe first implode graph? Although if the graph only has a few cycles this will not make it faster?
        addedEdges.foreach { case (source, targets) =>
            val sourceSCCOption = currSCCs.find(_.contains(source))
            (sourceSCCOption match {
                case None => targets
                case Some(scc) => targets.diff(scc)
            }).foreach { target =>
                val (paths, _) = findPaths(target, source)
                if paths.nonEmpty
                then
                    // Paths contains source and target.
                    val subSCCs = paths.flatMap(node => currSCCs.find(_.contains(node)))
                    val resultSCC = SmartUnion.sunion(subSCCs.flatten, paths)
                    currSCCs = (currSCCs -- subSCCs) + resultSCC
            }
        }

        currSCCs
