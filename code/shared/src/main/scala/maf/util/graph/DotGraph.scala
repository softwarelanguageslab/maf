package maf.util.graph

/** A graph representation that can be saved in a .dot file for visualization purposes */
case class DotGraph[N <: GraphElement, E <: GraphElement, S <: GraphElement]():
    class G2(
        val ids: Map[N, Int],
        val next: Int,
        val graphName: Option[String],
        val _nodes: Set[N],
        val _edges: Map[N, Set[(E, N)]],
        val _subgraphs: Map[S, Set[N]] = Map()): // Subgraphs are specified by their nodes, and have a specification.
        def _addNode(node: N): G2 =
            if _nodes.contains(node) then this
            else new G2(ids + (node -> next), next + 1, graphName, _nodes + node, _edges)
        private def _addEdgeNoCheck(
            node1: N,
            edge: E,
            node2: N
          ): G2 =
            if _edges.contains(node1) && _edges(node1).contains((edge, node2)) then this
            else
                val existing: Set[(E, N)] = _edges.getOrElse(node1, Set[(E, N)]())
                new G2(ids, next, graphName, _nodes, _edges + (node1 -> (existing ++ Set((edge, node2)))))
        def _addEdge(
            node1: N,
            edge: E,
            node2: N
          ): G2 =
            _addNode(node1)._addNode(node2)._addEdgeNoCheck(node1, edge, node2)
        def _addSubgraph(spec: S, sg: Set[N]): G2 =
            new G2(ids, next, graphName, _nodes, _edges, _subgraphs + (spec -> sg))

        def _setName(name: String): G2 = new G2(ids, next, Some(name), _nodes, _edges, _subgraphs)

        def toFile(path: String): Unit =
            withFileWriter(path)(out)

        private def withFileWriter(path: String)(body: java.io.Writer => Unit): Unit =
            val f = new java.io.File(path)
            val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
            body(bw)
            bw.close()

        def nodeString(n: N): String =
            val id = ids(n)
            val label =
                n.label.replace("<", "&lt;").nn.replace(">", "&gt;").nn.replace("&lt;br/&gt;", "<br/>").nn
            val color = n.color
            val shape = if n.shape == "" then "box" else n.shape
            val tooltip = n.metadata.toString.replace("<", "&lt;").nn.replace(">", "&gt;").nn
            val attr = if n.attributes.isEmpty then "" else n.attributes.map { case (k, v) => s"$k=$v" }.mkString(", ", ", ", "")
            s"node_$id[shape=$shape, label=<$label>, fillcolor=<$color> style=<filled>, tooltip=<$tooltip>$attr];\n" // Removed:   xlabel=$id,

        def edgeString(s: N, annot: E, t: N): String =
            val annotstr = annot.label
            val color = annot.color
            val shape = if annot.shape == "" then "none" else annot.shape
            val optionsText = (Map[String, String](
                "color" -> s"<$color>",
                "label" -> s"<$annotstr>",
                "constraint" -> (if annot.constrain then "true" else "false"),
                "shape" -> s"<$shape>"
            ) ++ annot.attributes).map { case (key, value) => s"$key=$value" }.mkString(",")
            s"node_${ids(s)} -> node_${ids(t)} [$optionsText]\n"

        def subgraphString(spec: S, nodes: Set[N]): String =
            /*
             * E.g.,
             *
             * subgraph cluster_0 {
             * style=filled;
             * color=lightgrey;
             * node [style=filled,color=white];
             * a0; a1;
             * label = "process #1";
             * }
             */
            val name = spec.attributes.getOrElse("name", "") // Bit of cheating.
            val nodeStyle = spec.attributes.get("nodeStyle") match {
                case Some(style) => s"node [$style];\n"
                case None => ""
            }
            val edgeStyle = spec.attributes.get("edgeStyle") match {
                case Some(style) => s"node [$style];\n"
                case None => ""
            }
            val attributes = spec.attributes + ("label" -> spec.attributes.getOrElse("label", "\"\"")) // Don't use the general plot label.
            val options = (attributes - "name" - "nodeStyle" - "edgeStyle").map{ case (key, value) => s"$key=$value;" }.mkString("","\n","\n")
            s"subgraph $name {\n" + // TODO, add attributes, specs of nodes and edges
                options +
                nodeStyle +
                edgeStyle +
                nodes.map(n => s"node_${ids(n)}").mkString(";") +
            "}\n"

        def graphNameString(name: Option[String]): String =
            name.map(n => s"label=\"$n\";\nlabelloc=\"t\";\nlabeljust=\"l\";\n").getOrElse("")

        def out(writer: java.io.Writer): Unit =
            writer.write("digraph G {\nsplines=ortho;\nnodesep=0.5;\nranksep=0.5;\n")
            writer.write(graphNameString(graphName))
            _subgraphs.foreach {s => writer.write(subgraphString(s._1, s._2))}
            _nodes.foreach {n => writer.write(nodeString(n))}
            _edges.foreach({ case (n1, ns) =>
                ns.foreach({ case (annot, n2) =>
                    writer.write(edgeString(n1, annot, n2))
                })
            })
            writer.write("}")

        def getNode(id: Int): Option[N] = ids.find({ case (_, v) => id == v }).map(_._1)

        def findNodes(p: N => Boolean) = _nodes.filter(p)

    object G2:
        implicit val typeclass: Graph[G2, N, E, S] = new Graph[G2, N, E, S] {
            def empty = new G2(Map[N, Int](), 0, None, Set[N](), Map[N, Set[(E, N)]]())
            def addNode(g: G2, node: N) = g._addNode(node)
            def addEdge(
                           g: G2,
                           node1: N,
                           edge: E,
                           node2: N
              ) = g._addEdge(node1, edge, node2)
            def removeNode(g: G2, node: N) = new G2(g.ids, g.next, g.graphName, g._nodes - node, g._edges)
            def addSubgraph(g: G2, spec: S, nodes: Set[N]): G2 = g._addSubgraph(spec, nodes)
            def removeEdge(
                              g: G2,
                              node1: N,
                              edge: E,
                              node2: N
              ) = ??? /* TODO[easy] implement */
            def nodes(g: G2) = g._nodes.size
            def edges(g: G2) = g._edges.size
            override def findNodeById(g: G2, id: Int): Option[N] =
                g.ids.collectFirst { case (n, _id) if id == _id => n }
            def findNodes(g: G2, p: N => Boolean) = ??? /* TODO[easy]: implement */
            def setName(g: G2, name: String) = g._setName(name)
        }

object SingleDotGraph extends DotGraph()

object DotGraph:
    def empty[N <: GraphElement, E <: GraphElement, S <: GraphElement] = new DotGraph[N, E, S]().G2.typeclass.empty

    private def convert(dotFile: String, typ: String, removeSource: Boolean = false): Boolean =
        if !dotFile.endsWith(".dot") then return false
        val source = dotFile.replace(" ", "\\ ").nn
        val target = source.dropRight(3) ++ typ
        import sys.process.*
        val result = s"dot -T$typ $source -o $target".! == 0
        if removeSource then s"rm $source".!
        result

    // Returns a boolean indicating whether the conversion was successful.
    def createPNG(dotFile: String, removeSource: Boolean = false): Boolean =
        convert(dotFile, "png", removeSource)

    def createSVG(dotFile: String, removeSource: Boolean = false): Boolean =
        convert(dotFile, "svg", removeSource)
