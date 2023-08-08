package maf.modular.incremental

import maf.core.Expression
import maf.modular.*
import maf.modular.scheme.LitAddr
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.*
import maf.util.graph.Colors.*

/** Provides functionalities to visualise the data flow graph that is inferred by the CY optimisation. */
trait IncrementalDataFlowVisualisation[Expr <: Expression] extends IncrementalGlobalStoreCY[Expr]:

    override def deleteComponent(cmp: Component): Unit =
        super.deleteComponent(cmp)

    sealed trait Edge:
        val source: Addr
        val target: Addr
        val bidirectional: Boolean
    case class ExplicitOrIntraCImplicit(source: Addr, target: Addr, bidirectional: Boolean = false) extends Edge
    case class InterComponentImplicit(source: Addr, target: Addr, bidirectional: Boolean = false) extends Edge

    def computeEdges(): Set[Edge] =
        // TODO: make this more efficient... Postpone the flatMapping until after the bidirifying (save edges as in dotgraph to have faster lookups).
        def bidirify[E <: Edge](edges: Set[E], make: (Addr, Addr, Boolean) => E) : Set[E] =
            var lst = edges
            var res: List[E] = List()
            while lst.nonEmpty
            do
                val first = lst.head
                lst = lst.tail
                lst.find(e => e.source == first.target && e.target == first.source) match
                    case Some(a) =>
                        res = make(first.source, first.target, true) :: res
                        lst = lst - a
                    case None =>
                        res = first :: res
            res.toSet
        val flowsR = explicitAndIntraComponentImplicitFlowsR()
        val interF = attachTransitiveFlowsToFlowsR(Map().withDefaultValue(Set()), computeTransitiveInterComponentFlows()) // Do not attach to explicit flows but keep separate.
        val e: Set[ExplicitOrIntraCImplicit] = flowsR.toList.flatMap {case (target, sources) => sources.map(s => ExplicitOrIntraCImplicit(s, target))}.toSet
        val i: Set[InterComponentImplicit] = interF.toList.flatMap {case (target, sources) => sources.map(s => InterComponentImplicit(s, target))}.toSet
        // Less arrows: if a flow exist in both directions, make 1 bidirectional arrow instead of 2.
        val eBidir = bidirify(e, {case (s, t, b) => ExplicitOrIntraCImplicit(s, t, b)})
        val iBidir = bidirify(i, {case (s, t, b) => InterComponentImplicit(s, t, b)})
        eBidir ++ iBidir


    /** Creates a dotgraph from the existing flow information and writes this to a file with the given filename. */
    def flowInformationToDotGraph(fileName: String): Unit =
        // Type of graph elements. One type suffices for both nodes and edges.
        case class GE(label: String,
                      color: Color = Colors.White,
                      override val shape: String = "",
                      metadata: GraphMetadata = GraphMetadataNone,
                      override val attributes: Map[String, String] = Map())
            extends GraphElement

        def addrToGE(a: Addr): GE = a match {
            case _: LitAddr[_] => GE(a.toString, Colors.PinkOrange, "octagon")
            case _ => GE(a.toString, shape = "box")
        }

        def edgeToGE(e: Edge): GE =
            val colour = e match {
                case _: ExplicitOrIntraCImplicit => Colors.Bordeaux
                case _: InterComponentImplicit => Colors.DarkGrey
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
          val colour = palette(index)
          val attributes = Map("name" -> s"cluster_$index","style" -> "filled", "color" -> s"<$colour>")
          val ge_nodes = sca.map(nodes)
          val ge_subgraph = GE("",  attributes = attributes)
          (ge_subgraph, ge_nodes)
        })
        // Create the graph and write it to a file. Only edges that are assigned a colour will be drawn.
        val g = DotGraph[GE, GE, GE]().G.typeclass
        subgraphs.foldLeft(edges.foldLeft(nodes.values.foldLeft(g.empty) { case (graph, node: GE) => g.addNode(graph, node) }) {
            case (graph, edge) => g.addEdge(graph, nodes(edge.source), edgeToGE(edge), nodes(edge.target))
        }){ case (graph, (sg, nodes)) => g.addSubgraph(graph, sg, nodes)}.toFile(fileName)

    def dataFlowToImage(fileName: String): Unit =
        flowInformationToDotGraph(fileName)
        if !DotGraph.createSVG(fileName, true)
        then System.err.nn.println("Conversion failed.")
