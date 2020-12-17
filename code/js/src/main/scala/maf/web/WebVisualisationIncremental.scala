package maf.web

import maf.core.Expression
import maf.modular.DependencyTracking
import maf.modular.incremental.IncrementalModAnalysis
import maf.web.WebVisualisation._
import maf.web.WebVisualisationIncremental._
import scala.scalajs.js

trait VisualisableIncrementalModAnalysis[Expr <: Expression] extends IncrementalModAnalysis[Expr] with DependencyTracking[Expr] {
  var recursive: Set[Component] = Set() // Collects the set of recursive components, since self-edges are omitted in the set `cachedSpawns`.

  trait VisualisableIntraAnalysis extends IncrementalIntraAnalysis with DependencyTrackingIntra {
    override def refineComponents(): Unit = {
      if (C.contains(component)) recursive = recursive + component
      else recursive = recursive - component
      super.refineComponents()
    }
  }
}

object WebVisualisationIncremental {
  val __CSS_DELETED_NODE__ = "node_deleted"
  val __CSS_DELETED_EDGE__ = "edge_deleted"
  val __SVG_DELETED_ARROW__ = "endarrow_deleted"
  val __CSS_MAIN_NODE__ = "node_main"
}

class WebVisualisationIncremental[Expr <: Expression](override val analysis: VisualisableIncrementalModAnalysis[Expr])
  extends WebVisualisation(analysis) {

  def deletedComponent(cmp: analysis.Component): Boolean = !analysis.visited.contains(cmp) && !analysis.workList.contains(cmp)

  override def classifyNodes(): Unit = {
    super.classifyNodes()
    nodes.classed(__CSS_DELETED_NODE__, (node: Node) => deletedComponent(node.component))
    nodes.classed(__CSS_MAIN_NODE__, (node: Node) => node.component == analysis.initialComponent)
  }

  override def classifyEdges(): Unit = {
    super.classifyEdges()
    edges.classed(__CSS_DELETED_EDGE__, (edge: Edge) =>
      deletedComponent(edge.source.component)
        || deletedComponent(edge.target.component)
        || (!analysis.dependencies(edge.source.component).contains(edge.target.component)) // The edge has been deleted.
              && !(edge.source.component == edge.target.component && analysis.recursive.contains(edge.source.component))) // Check that it is not a currently existing self-edge.
  }

  /**
   * Ensures all new nodes and edges in the analysis are drawn. May not shown nodes that have ceased to exist.
   * @note No nodes and edges are deleted. Those that are deleted by the analysis will still be drawn, but in will be visualised differently.
   * @note Nodes and edges that were created and deleted by the analysis since the last update of the visualisation data will not be shown using this method.
   */
  override def refreshData(): Unit = {
    // Add all components currently in the analysis.
    analysis.visited.foreach { cmp =>
      val node = getNode(cmp)
      nodesData += node
    }
    // Add all edges currently in the analysis.
    nodesData.foreach { sourceNode =>
      val targets = analysis.dependencies(sourceNode.component)
      targets.foreach(target => {
        val targetNode = getNode(target)
        val edge = getEdge(sourceNode,targetNode)
        edgesData += edge
      })
    }
  }

  override def refreshDataAfterStep(cmp: analysis.Component, oldDeps: Set[analysis.Component]): Unit = {
    val sourceNode = getNode(cmp)
    // Add new edges.
    analysis.dependencies(cmp).foreach { otherCmp =>
      val targetNode = getNode(otherCmp)
      val edge = getEdge(sourceNode,targetNode)
      nodesData += targetNode
      edgesData += edge
    }
  }

  override def setupMarker(svg: JsAny): js.Dynamic = {
    super.setupMarker(svg)
    val marker = svg.select("defs").append("marker")
                                   .attr("id",__SVG_DELETED_ARROW__)
                                   .attr("viewBox","-0 -5 10 10")
                                   .attr("refX",0)
                                   .attr("refY",0)
                                   .attr("orient","auto")
                                   .attr("markerWidth",5)
                                   .attr("markerHeight",5)
                                   .attr("fill","darkgray")
                                   .attr("stroke","darkgray")
    marker.append("svg:path").attr("d", "M 0,-5 L 10 ,0 L 0,5")
  }

}
