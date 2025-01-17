package maf.modular.incremental

import maf.core.Expression
import maf.modular.*
import maf.modular.scheme.*
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.*
import maf.util.graph.Colors.*

/** Provides functionalities to visualise the data flow graph that is inferred by the CY optimisation. */
trait IncrementalDataFlowVisualisation[Expr <: Expression] extends IncrementalGlobalStoreCY[Expr]:

    override def deleteComponent(cmp: Component): Unit =
        super.deleteComponent(cmp)

    case class Edge(source: Addr, target: Addr, bidirectional: Boolean = false)

    def computeEdges(): Set[Edge] =
        // TODO: make this more efficient... Postpone the flatMapping until after the bidirifying (save edges as in dotgraph to have faster lookups).
        def bidirify(edges: Set[Edge]): Set[Edge] =
            var lst = edges
            var res: List[Edge] = List()
            while lst.nonEmpty
            do
                val first = lst.head
                lst = lst.tail
                lst.find(e => e.source == first.target && e.target == first.source) match
                    case Some(a) =>
                        res = Edge(first.source, first.target, true) :: res
                        lst = lst - a
                    case None =>
                        res = first :: res
            res.toSet
        val i: Set[Edge] = computeInformationFlow().toList.flatMap {case (target, sources) => sources.map(s => Edge(s, target))}.toSet
        // Less arrows: if a flow exist in both directions, make 1 bidirectional arrow instead of 2.
        bidirify(i)

    /** Creates a dotgraph from the existing flow information and writes this to a file with the given filename. */
    def flowInformationToDotGraph(fileName: String, name: Option[String] = None): Unit =
        // Type of graph elements. One type suffices for both nodes and edges.
        case class GE(label: String,
                      color: Color = Colors.White,
                      override val shape: String = "",
                      metadata: GraphMetadata = GraphMetadataNone,
                      override val attributes: Map[String, String] = Map())
            extends GraphElement

        def addrToGE(a: Addr): GE = a match {
            case _: LitAddr[_] => GE(a.toString, Colors.PinkOrange, "octagon")
            case _: FlowAddr[_] => GE(a.toString, Colors.Grey, "oval")
            case _ => GE(a.toString, shape = "box")
        }

        def edgeToGE(e: Edge): GE =
            val colour = e.source match {
                case _: LitAddr[_] => Colors.Bordeaux
                case _: FlowAddr[_] => Colors.DarkGrey
                case _ => Colors.RoseEbony
            }
            val attributes = if e.bidirectional then Map("style" -> "\"dashed\"", "arrowhead" -> "none") else Map()
            GE("", colour, attributes = attributes)

        // Compute the edges.
        val edges: Set[Edge] = computeEdges()
        // Generate the nodes. Create a mapping from addresses to graph elements (nodes).
        // IMPORTANT: if a node has no edges, it will be omitted...
        val nodes: Map[Addr, GE] = (edges.map(_.source) ++ edges.map(_.target)).map(a => (a, addrToGE(a))).toMap
        // Compute a subgraph for every SCA.
        val subgraphs: List[(GE, Set[GE])] = computeSCAs().toList.zipWithIndex.map({case (sca, index) =>
          val attributes = Map("name" -> s"cluster_$index","style" -> "\"filled,dashed\"", "fillcolor" -> s"<${Colors.Yellow}>")
          val ge_nodes = sca.map(nodes)
          val ge_subgraph = GE("",  attributes = attributes)
          (ge_subgraph, ge_nodes)
        })
        // Create the graph and write it to a file. Only edges that are assigned a colour will be drawn.
        val g = DotGraph[GE, GE, GE]().G2.typeclass
        val empty = name match
            case Some(n) => g.setName(g.empty, n)
            case None => g.empty
        subgraphs.foldLeft(edges.foldLeft(nodes.values.foldLeft(empty) { case (graph, node: GE) => g.addNode(graph, node) }) {
            case (graph, edge) => g.addEdge(graph, nodes(edge.source), edgeToGE(edge), nodes(edge.target))
        }){ case (graph, (sg, nodes)) => g.addSubgraph(graph, sg, nodes)}.toFile(fileName)
        println(s"Graph $fileName contains ${edges.size} edges and ${nodes.size} nodes and ${subgraphs.size} SCAs.")

    def dataFlowToImage(fileName: String, name: Option[String] = None): Unit =
        flowInformationToDotGraph(fileName, name)
        if !DotGraph.createSVG(fileName, true)
        then System.err.nn.println("Conversion failed.")
