package maf.util.graph

object AcyclicerTester:
    def isAcyclic[N](nodes: List[N], edges: Map[N, Set[N]]): Boolean =
        val dfs: DFS[N] = DFS(nodes, edges)
        var isCyclic = false
        dfs.run

        !nodes.exists(from => edges.get(from).getOrElse(Set()).exists((to) => dfs.departure(from) <= dfs.departure(to)))
