package maf.util.graph

class DFS[N](nodes: List[N], edges: Map[N, Set[N]]):
    private var visited: Set[N] = Set()
    private var order: Map[N, Int] = Map()
    private var counter = 0
    private var onBump: (N, N) => Boolean = (_, _) => true

    private def neighbours(n: N): Set[N] =
        edges.get(n).getOrElse(Set())

    /**
     * Add a callback that will be called when we bump into a node (a node that was already visited)
     *
     * @param f
     *   a function from two nodes (from, to) to a boolean. The boolean should be false if the DFS should not continue, otherwise true.
     */
    def addBump(f: (N, N) => Boolean): Unit =
        onBump = f

    def departure(n: N): Int = order(n)

    /** Do the depth-first-traversal, number each node as we visit them */
    def run: Unit =
        nodes.foreach { n =>
            if !visited.contains(n) then dfs(n)
        }

    private def dfs(node: N): Unit =
        // mark the node as visited
        visited += node
        // then DFS its children
        neighbours(node).foreach { n =>
            if visited.contains(n) then onBump(node, n)
            else dfs(n)
        }
        // increase the departure counter
        counter = counter + 1
        // give it an order
        order = order + (node -> counter)
