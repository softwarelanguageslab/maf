package maf.util.graph

object Traclo:

    /** Implementation of Warshall's transitive closure algorithm.
     * Implementation based on W. De Meuter "Algorithms and Data Structures in Scheme".<br> 
     * The algorithm operates in O(|V|^3^).
     * @param nodes
     *   The set of nodes in the graph.
     * @param edges
     *   A map, mapping each node `from` to a set of nodes `to` for which the graph contains an edge `from -> to`.
     * @tparam Node
     *   The type of a node.
     * @return
     *   An updated set of edges, being the transitive closure.
     */
    def warshall[Node](nodes: Set[Node], e: Map[Node, Set[Node]]): Map[Node, Set[Node]] =
        var edges = e
        nodes.foreach({ via =>
           nodes.foreach({ from =>
             if edges(from).contains(via)
             then
                edges(via).foreach({ to =>
                   edges = edges + (from -> (edges(from) + to))
                })
            })
        })
        edges

