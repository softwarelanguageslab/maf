package maf.util.graph

import scala.collection.mutable

/** Does a topologically sorting of the given directed acyclic graph */
object TopSort:
    def topsort[N](nodes: List[N], edges: Map[N, Set[N]]): List[N] =
        assert(AcyclicerTester.isAcyclic(nodes, edges), "graph must be acyclic")

        var visited: Set[N] = Set()
        var topsort: List[N] = List()
        var indegrees: Map[N, Int] = edges.foldLeft(Map().withDefaultValue(0)) { case (indegrees, (_, tos)) =>
            tos.foldLeft(indegrees)((indegrees, to) => indegrees + (to -> (indegrees(to) + 1)))
        }

        val fifo: mutable.Queue[N] = mutable.Queue()

        nodes.foreach(n => if indegrees(n) == 0 then fifo.enqueue(n))

        while (fifo.nonEmpty) do
            val n = fifo.dequeue
            if !visited.contains(n) then
                visited = visited + n
                topsort = n :: topsort
                edges
                    .get(n)
                    .getOrElse(Set())
                    .foreach(to =>
                        indegrees = indegrees + (to -> (indegrees(to) - 1))
                        if indegrees(to) == 0 then fifo.enqueue(to)
                    )

        topsort.reverse
