package maf.modular.incremental

import maf.core.Expression
import maf.modular.*

trait IncrementalDataFlowVisualisation[Expr <: Expression] extends IncrementalGlobalStore[Expr]:

    case class AdrDep(a: Addr, directFlow: Boolean, indirectFlow: Boolean) // Keep track of the type of flow.
    var addressDependenciesLog: Map[Component, Map[Addr, Set[AdrDep]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    trait IncrementalVisualIntra extends IncrementalGlobalStoreIntraAnalysis:

        override def writeAddr(addr: Addr, value: Value): Boolean =
            if configuration.cyclicValueInvalidation then
                var flattenedFlows = implicitFlows.flatten.toSet
                var set = lattice.getAddresses(value).map(a => AdrDep(a, true, flattenedFlows(a))).toSet
                flattenedFlows = flattenedFlows.filter(adr => set.find(_.a == adr).isEmpty)
                set = set ++ flattenedFlows.map(a => AdrDep(a, false, true))
                addressDependenciesLog = addressDependenciesLog + (component -> (addressDependenciesLog(component) + (addr -> set)))
            val b = super.writeAddr(addr, value)
            b

    end IncrementalVisualIntra

    def flowInformationToDotGraph(fileName: String): Unit =
        import maf.util.graph.*
        import maf.util.graph.Colors.*
        // Type of graph elements.
        case class GE(label: String, color: Color = Colors.White, metadata: GraphMetadata = GraphMetadataNone) extends GraphElement
        // Colour nodes that are part of a SCA.
        val nodeColors = computeSCAs().toList.zipWithIndex
          .flatMap({ case (sca, index) => val color = allColors(index); sca.map(v => (v, color)) })
          .toMap
          .withDefaultValue(Colors.White)
        // Generate the nodes.
        val nodes: Map[Addr, GE] =
          (addressDependenciesLog.values.flatMap(_.keySet) ++ addressDependenciesLog.values.flatMap(_.values).flatten.map(_.a).toSet)
            .map(addr => (addr, GE(addr.toString(), nodeColors(addr))))
            .toMap
        // Compute the edges.
        val edges: Set[(GE, GE, AdrDep)] =
          addressDependenciesLog.values.flatten.flatMap({ case (w, rs) => rs.map(r => (nodes(r.a), nodes(w), r)) }).toSet
        // Create the graph and write it to a file.
        val g = DotGraph[GE, GE]().G.typeclass
        edges
          .foldLeft(nodes.values.foldLeft(g.empty) { case (graph, node: GE) => g.addNode(graph, node) }) {
            case (graph, (source: GE, target: GE, adrDep: AdrDep)) =>
              val color = (adrDep.directFlow, adrDep.indirectFlow) match {
                case (true, true)  => Colors.DarkBlue // Direct and indirect flows
                case (true, false) => Colors.Black // Direct flows
                case (false, true) => Colors.Grey // Indirect flows

              }
              g.addEdge(graph, source, GE("", color), target)
          }
          .toFile(fileName)
