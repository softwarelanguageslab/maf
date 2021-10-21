package maf.modular.incremental

import maf.core.Expression
import maf.modular.*
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.*
import maf.util.graph.Colors.*

/** Provides functionalities to visualise the data flow graph that is inferred by the CY optimisation. */
trait IncrementalDataFlowVisualisation[Expr <: Expression] extends IncrementalGlobalStore[Expr]:

    private case class AdrDep(a: Addr, directFlow: Boolean, indirectFlow: Boolean) // Keep track of the type of flow.
    // Keep track of dependencies. Similar to the data structure in IncrementalGlobalStore, but keeps track of the kind of flow.
    private var addressDependenciesLog: Map[Component, Map[Addr, Set[AdrDep]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    override def deleteComponent(cmp: Component): Unit =
        if configuration.cyclicValueInvalidation then addressDependenciesLog = addressDependenciesLog - cmp
        super.deleteComponent(cmp)

    trait IncrementalVisualIntra extends IncrementalGlobalStoreIntraAnalysis:

        abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit =
            if configuration.cyclicValueInvalidation then addressDependenciesLog = addressDependenciesLog - component
            super.analyzeWithTimeout(timeout)

        override def writeAddr(addr: Addr, value: Value): Boolean =
            if configuration.cyclicValueInvalidation then
                var flattenedFlows = implicitFlows.flatten.toSet
                var set = lattice.getAddresses(value).map(a => AdrDep(a, true, flattenedFlows(a))).toSet
                flattenedFlows = flattenedFlows.filter(adr => set.find(_.a == adr).isEmpty)
                set = set ++ flattenedFlows.map(a => AdrDep(a, false, true))
                var newDependencies = SmartUnion.sunion(addressDependenciesLog(component)(addr), set)
                addressDependenciesLog = addressDependenciesLog + (component -> (addressDependenciesLog(component) + (addr -> newDependencies)))
            val b = super.writeAddr(addr, value)
            b

    end IncrementalVisualIntra

    /** Computes the color of an edge based on its specifications. */
    private def edgeColor(directFlow: Boolean, indirectFlow: Boolean): Option[Color] = (directFlow, indirectFlow) match {
      case (true, true)  => Some(Colors.DarkBlue) // Both direct and indirect flows
      case (true, false) => Some(Colors.Black) // Direct flows
      case (false, true) => Some(Colors.Grey) // Indirect flows
      case _             => Some(Colors.Red) // No flow, should not happen...
    }

    /** Creates a dotgraph from the existing flow information and writes this to a file with the given filename. */
    def flowInformationToDotGraph(fileName: String): Unit =
        // Type of graph elements. One type suffices for both nodes and edges.
        case class GE(label: String, color: Color = Colors.White, metadata: GraphMetadata = GraphMetadataNone) extends GraphElement
        // Colour nodes by SCA.
        val nodeColors = computeSCAs().toList.zipWithIndex
          .flatMap({ case (sca, index) => val color = allColors(index); sca.map(v => (v, color)) })
          .toMap
          .withDefaultValue(Colors.White)
        // Generate the nodes. Create a mapping from addresses to graph elements (nodes).
        val nodes: Map[Addr, GE] =
          (addressDependenciesLog.values.flatMap(_.keySet) ++ addressDependenciesLog.values.flatMap(_.values).flatten.map(_.a).toSet)
            .map(addr => (addr, GE(addr.toString(), nodeColors(addr))))
            .toMap
        // Compute the edges.
        val edges: Set[(GE, GE, AdrDep)] =
          addressDependenciesLog.values.flatten.flatMap({ case (w, rs) => rs.map(r => (nodes(r.a), nodes(w), r)) }).toSet
        // Create the graph and write it to a file. Only edges that are assigned a colour will be drawn.
        val g = DotGraph[GE, GE]().G.typeclass
        edges
          .foldLeft(nodes.values.foldLeft(g.empty) { case (graph, node: GE) => g.addNode(graph, node) }) {
            case (graph, (source: GE, target: GE, adrDep: AdrDep)) =>
              edgeColor(adrDep.directFlow, adrDep.indirectFlow).map(color => g.addEdge(graph, source, GE("", color), target)).getOrElse(graph)
          }
          .toFile(fileName)
